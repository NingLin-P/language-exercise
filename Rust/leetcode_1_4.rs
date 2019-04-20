truct Solution;

// 1. Two Sum
use std::collections::HashMap;

impl Solution {
    pub fn two_sum(nums: Vec<i32>, target: i32) -> Vec<i32> {
        let mut map = HashMap::with_capacity(nums.len());
        for (index, num) in nums.iter().enumerate() {
            if let Some(sub_index) = map.get(num) {
                return vec![*sub_index, index as i32];
            }
            map.insert(target - num, index as i32);
        }
        vec![]
    }
}

// 2. Add Two Numbers
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ListNode {
    pub val: i32,
    pub next: Option<Box<ListNode>>,
}

impl ListNode {
    #[inline]
    fn new(val: i32) -> Self {
        ListNode { next: None, val }
    }
}

impl Solution {
    pub fn add_two_numbers(l1: Option<Box<ListNode>>, l2: Option<Box<ListNode>>) -> Option<Box<ListNode>> {
        fn add_two_nums_plus_one( l1: Option<Box<ListNode>>, l2: Option<Box<ListNode>>, plus: i32) -> Option<Box<ListNode>> {
            let (val, n1, n2) = match (l1, l2) {
                (Some(n1), Some(n2)) => (n1.val + n2.val + plus, n1.next, n2.next),
                (Some(n), None) => (n.val + plus, n.next, None),
                (None, Some(n)) => (n.val + plus, None, n.next),
                _ => (plus, None, None),
            };
            if val == 0 && n1 == None && n2 == None {
                return None;
            }
            let next_plus = if val >= 10 { 1 } else { 0 };
            Some(Box::new(ListNode {
                val: val % 10,
                next: add_two_nums_plus_one(n1, n2, next_plus),
            }))
        }
        add_two_nums_plus_one(l1, l2, 0).or_else(|| Some(Box::new(ListNode::new(0))))
    }
}

// 3. Longest Substring Without Repeating Characters
impl Solution {
    pub fn length_of_longest_substring(s: String) -> i32 {
        let cs: Vec<_> = s.chars().collect();
        let len = cs.len();
        let mut max = 0;
        let mut head = 0;
        let mut end = 0;

        while end < len {
            for i in head..end {
                if cs[i] == cs[end] {
                    head = i + 1;
                    break;
                }
            }
            if max < end - head + 1 {
                max = end - head + 1;
            }
            end += 1;
        }
        return max as i32;
    }
}

// 4. Median of Two Sorted Arrays
impl Solution {
    pub fn find_median_sorted_arrays(nums1: Vec<i32>, nums2: Vec<i32>) -> f64 {
        let mut nums1 = nums1;
        nums1.extend(nums2.into_iter());
        nums1.sort();
        let len = nums1.len();
        if len % 2 == 1 {
            nums1[len / 2] as f64
        } else {
            ((nums1[len / 2 - 1] + nums1[len / 2]) as f64) / 2.0
        }
    }
}

