use crate::geodata::importer::OsmRef;

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct NodePos {
    lat: f64,
    lon: f64,
}

impl NodePos {
    pub(crate) fn new(lat: f64, lon: f64) -> Self {
        Self { lat, lon }
    }

    fn default() -> Self {
        Self { lat: 0.0, lon: 0.0 }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Way {
    nodes: Vec<OsmRef>,
    first: NodePos,
    last: NodePos,
}

impl Way {
    pub(crate) fn new(nodes: Vec<OsmRef>, first: NodePos, last: NodePos) -> Self {
        Self { nodes, first, last }
    }

    pub(crate) fn is_begin(&self, other_node: &NodePos) -> bool {
        self.first == *other_node
    }

    pub(crate) fn is_end(&self, other_node: &NodePos) -> bool {
        self.last == *other_node
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Polygon {
    nodes: Vec<OsmRef>,
    first: NodePos,
    last: NodePos,
}

impl Polygon {
    fn new() -> Self {
        Self {
            nodes: Vec::new(),
            first: NodePos::default(),
            last: NodePos::default(),
        }
    }

    fn assimilate_way(&mut self, way: &Way) -> bool {
        if self.nodes.is_empty() {
            self.first = way.first.clone();
            self.last = way.last.clone();
            self.nodes.append(&mut way.nodes.clone());
        } else if way.is_begin(&self.last) {
            self.last = way.last.clone();
            for i in 1..way.nodes.len() {
                self.nodes.push(way.nodes[i].clone())
            }
        } else if way.is_end(&self.first) {
            self.first = way.first.clone();
            for i in (0..way.nodes.len() - 1).rev() {
                self.nodes.insert(0, way.nodes[i].clone())
            }
        } else if way.is_begin(&self.first) {
            self.first = way.last.clone();
            for i in 1..way.nodes.len() {
                self.nodes.insert(0, way.nodes[i].clone())
            }
        } else if way.is_end(&self.last) {
            self.last = way.first.clone();
            for i in (0..(way.nodes.len() - 1)).rev() {
                self.nodes.push(way.nodes[i].clone())
            }
        } else {
            return false;
        }
        true
    }

    fn assimilate_polygon(&mut self, polygon: &Polygon) -> bool {
        if self.first == polygon.last {
            // linear prepend
            self.first = polygon.first.clone();
            for i in (0..(polygon.nodes.len() - 1)).rev() {
                self.nodes.insert(0, polygon.nodes[i]);
            }
        } else if self.last == polygon.first {
            // linear append
            self.last = polygon.last.clone();
            for i in 1..polygon.nodes.len() {
                self.nodes.push(polygon.nodes[i]);
            }
        } else if self.first == polygon.first {
            // reverse prepend
            self.first = polygon.last.clone();
            for i in 1..polygon.nodes.len() {
                self.nodes.insert(0, polygon.nodes[i]);
            }
        } else if self.last == polygon.last {
            // reverse append
            self.last = polygon.first.clone();
            for i in (0..(polygon.nodes.len() - 1)).rev() {
                self.nodes.push(polygon.nodes[i]);
            }
        } else {
            return false;
        }
        true
    }

    fn absorb_polygon(&mut self, polygon: &Polygon) {
        self.last = polygon.last.clone();
        for i in 0..polygon.nodes.len() {
            self.nodes.push(polygon.nodes[i]);
        }
    }

    fn is_closed(&self) -> bool {
        !self.nodes.is_empty() && self.first == self.last
    }

    fn feed_with_ways(&mut self, mut ways: Vec<Way>) -> Vec<Way> {
        let mut remaining_ways = Vec::new();

        while !ways.is_empty() {
            let way = ways.pop().unwrap();

            if self.is_closed() || !self.assimilate_way(&way) {
                remaining_ways.push(way);
            }
        }

        remaining_ways
    }

    fn feed_with_polygons(&mut self, mut polygons: Vec<Polygon>) -> Vec<Polygon> {
        let mut remaining_polygons = Vec::new();

        while !polygons.is_empty() {
            let poly = polygons.pop().unwrap();

            if self.is_closed() || !self.assimilate_polygon(&poly) {
                remaining_polygons.push(poly);
            }
        }

        remaining_polygons
    }

    pub(crate) fn nodes(&self) -> Vec<OsmRef> {
        self.nodes.clone()
    }
}

pub(crate) fn ways_to_polygons(mut ways: Vec<Way>) -> Vec<Polygon> {
    let mut finished_polygons = Vec::new();
    let mut unfinished_polygons = Vec::new();

    loop {
        let mut poly = Polygon::new();

        if ways.is_empty() {
            break;
        }

        ways = poly.feed_with_ways(ways);
        if poly.is_closed() {
            finished_polygons.push(poly);
        } else {
            unfinished_polygons.push(poly);
        }
    }

    if unfinished_polygons.len() == 1 {
        let mut poly = unfinished_polygons.pop().unwrap();

        poly.nodes.push(poly.nodes.first().unwrap().clone());
        finished_polygons.push(poly);
    }

    let mut orphans_polygons = Vec::new();
    while !unfinished_polygons.is_empty() {
        let mut poly = unfinished_polygons.pop().unwrap();
        unfinished_polygons = poly.feed_with_polygons(unfinished_polygons.clone());

        if poly.is_closed() {
            finished_polygons.push(poly);
        } else {
            orphans_polygons.push(poly);
        }
    }

    if !orphans_polygons.is_empty() {
        let mut poly = orphans_polygons.pop().unwrap();

        while !orphans_polygons.is_empty() {
            poly.absorb_polygon(&orphans_polygons.pop().unwrap());
        }

        if !poly.is_closed() {
            poly.nodes.push(poly.nodes.first().unwrap().clone());
            finished_polygons.push(poly);
        }
    }

    finished_polygons
}

#[test]
fn test_collect_polygons_simple() {
    let nodes = vec![
        NodePos::new(0.0, 0.0),
        NodePos::new(20.0, 10.0),
        NodePos::new(20.0, 20.0),
        NodePos::new(10.0, 40.0),
        NodePos::new(5.0, 20.0),
        NodePos::new(5.0, 10.0),
        NodePos::new(10.0, 5.0),
    ];

    let way = |first_id, last_id| -> Way {
        Way::new(
            vec![first_id, last_id],
            nodes[first_id as usize].clone(),
            nodes[last_id as usize].clone(),
        )
    };

    let ways = vec![way(6, 1), way(1, 2), way(3, 2), way(3, 4), way(5, 4), way(6, 5)];
    let expected_nodes_order = vec![1, 6, 5, 4, 3, 2, 1];

    let polygons = ways_to_polygons(ways);

    assert_eq!(vec![polygons[0].nodes()], vec![expected_nodes_order]);
}

#[test]
fn test_collect_polygons_two() {
    let nodes = vec![
        NodePos::new(0.0, 0.0),
        NodePos::new(20.0, 10.0),
        NodePos::new(20.0, 20.0),
        NodePos::new(10.0, 40.0),
        NodePos::new(5.0, 20.0),
        NodePos::new(5.0, 10.0),
        NodePos::new(10.0, 5.0),
        NodePos::new(20.0 * 2.0, 10.0 * 2.0),
        NodePos::new(20.0 * 2.0, 20.0 * 2.0),
        NodePos::new(10.0 * 2.0, 40.0 * 2.0),
        NodePos::new(5.0 * 2.0, 20.0 * 2.0),
        NodePos::new(5.0 * 2.0, 10.0 * 2.0),
        NodePos::new(10.0 * 2.0, 5.0 * 2.0),
    ];

    let way = |first_id, last_id| -> Way {
        Way::new(
            vec![first_id, last_id],
            nodes[first_id as usize].clone(),
            nodes[last_id as usize].clone(),
        )
    };

    let ways = vec![
        way(6, 1),
        way(1, 2),
        way(3, 2),
        way(3, 4),
        way(5, 4),
        way(6, 5),
        way(6 + 6, 1 + 6),
        way(1 + 6, 2 + 6),
        way(3 + 6, 2 + 6),
        way(3 + 6, 4 + 6),
        way(5 + 6, 4 + 6),
        way(6 + 6, 5 + 6),
    ];
    let expected_nodes_order_first = vec![5, 6, 1, 2, 3, 4, 5];
    let expected_nodes_order_second = vec![1 + 6, 6 + 6, 5 + 6, 4 + 6, 3 + 6, 2 + 6, 1 + 6];

    let polygons = ways_to_polygons(ways);

    assert_eq!(
        vec![polygons[1].nodes(), polygons[0].nodes()],
        vec![expected_nodes_order_first, expected_nodes_order_second]
    );
}

#[test]
fn test_collect_polygons_holes() {
    let nodes = vec![
        NodePos::new(0.0, 0.0),
        NodePos::new(20.0, 10.0),
        NodePos::new(20.0, 20.0),
        NodePos::new(10.0, 40.0),
        NodePos::new(5.0, 20.0),
        NodePos::new(5.0, 10.0),
        NodePos::new(10.0, 5.0),
        NodePos::new(3.0, 3.0),
    ];

    let way = |first_id, last_id| -> Way {
        Way::new(
            vec![first_id, last_id],
            nodes[first_id as usize].clone(),
            nodes[last_id as usize].clone(),
        )
    };

    let ways = vec![way(6, 1), way(3, 2), way(3, 4), way(0, 6), way(7, 5), way(7, 0)];
    let expected_nodes_order = vec![5, 7, 0, 6, 1, 4, 3, 2, 5];

    let polygons = ways_to_polygons(ways);

    assert_eq!(vec![polygons[0].nodes()], vec![expected_nodes_order]);
}

#[test]
fn test_collect_polygons_shuffle() {
    let nodes = vec![
        NodePos::new(0.0, 0.0),
        NodePos::new(20.0, 10.0),
        NodePos::new(20.0, 20.0),
        NodePos::new(10.0, 40.0),
        NodePos::new(5.0, 20.0),
        NodePos::new(5.0, 10.0),
        NodePos::new(10.0, 5.0),
        NodePos::new(3.0, 3.0),
    ];

    let way = |first_id, last_id| -> Way {
        Way::new(
            vec![first_id, last_id],
            nodes[first_id as usize].clone(),
            nodes[last_id as usize].clone(),
        )
    };

    let ways = vec![
        way(1, 2),
        way(6, 1),
        way(3, 2),
        way(3, 4),
        way(0, 6),
        way(7, 5),
        way(7, 0),
        way(5, 4),
    ];
    let expected_nodes_order = vec![1, 2, 3, 4, 5, 7, 0, 6, 1];

    let polygons = ways_to_polygons(ways);

    assert_eq!(vec![polygons[0].nodes()], vec![expected_nodes_order]);
}

#[test]
fn test_collect_polygons_shuffle_twice() {
    let nodes = vec![
        NodePos::new(0.0, 0.0),
        NodePos::new(20.0, 10.0),
        NodePos::new(20.0, 20.0),
        NodePos::new(10.0, 40.0),
        NodePos::new(5.0, 20.0),
        NodePos::new(5.0, 10.0),
        NodePos::new(10.0, 5.0),
        NodePos::new(3.0, 3.0),
        NodePos::new(0.0, 0.0),
        NodePos::new(0.0, 0.0),
        NodePos::new(33.0, 33.0),
        NodePos::new(20.0 + 100.0, 10.0 + 100.0),
        NodePos::new(20.0 + 100.0, 20.0 + 100.0),
        NodePos::new(10.0 + 100.0, 40.0 + 100.0),
        NodePos::new(5.0 + 100.0, 20.0 + 100.0),
        NodePos::new(5.0 + 100.0, 10.0 + 100.0),
        NodePos::new(10.0 + 100.0, 5.0 + 100.0),
        NodePos::new(3.0 + 100.0, 3.0 + 100.0),
    ];

    let way = |first_id, last_id| -> Way {
        Way::new(
            vec![first_id, last_id],
            nodes[first_id as usize].clone(),
            nodes[last_id as usize].clone(),
        )
    };

    let ways = vec![
        way(1, 2),
        way(6, 1),
        way(3, 2),
        way(3, 4),
        way(0, 6),
        way(7, 5),
        way(7, 0),
        way(5, 4),
        way(1 + 10, 2 + 10),
        way(6 + 10, 1 + 10),
        way(3 + 10, 2 + 10),
        way(3 + 10, 4 + 10),
        way(0 + 10, 6 + 10),
        way(7 + 10, 5 + 10),
        way(7 + 10, 0 + 10),
        way(5 + 10, 4 + 10),
    ];
    let expected_nodes_order_first = vec![5, 4, 3, 2, 1, 6, 0, 7, 5];
    let expected_nodes_order_second = vec![11, 16, 10, 17, 15, 14, 13, 12, 11];

    let polygons = ways_to_polygons(ways);

    assert_eq!(polygons[0].nodes(), expected_nodes_order_first);
    assert_eq!(polygons[1].nodes(), expected_nodes_order_second);
}
