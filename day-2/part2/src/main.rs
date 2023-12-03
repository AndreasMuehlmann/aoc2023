use std::collections::HashMap;
use std::fs;
use std::path::Path;

fn get_cube_counts(cube_set: String) -> HashMap<String, u32> {
    let mut cube_counts: HashMap<String, u32> = HashMap::new();
    for cube_string in cube_set.split(',') {
        let trimmed_start_cube_string: &str = cube_string.trim();
        let count_color: Vec<&str> = trimmed_start_cube_string.split_whitespace().collect();
        let cube_count: u32 = count_color[0].parse::<u32>().unwrap();
        cube_counts.insert(count_color[1].to_string(), cube_count);
    }
    return cube_counts;
}

fn get_max_cube_counts_game(cube_sets: &str) -> HashMap<String, u32> {
    let mut max_cube_counts: HashMap<String, u32> = HashMap::new();
    for cube_set in cube_sets.split(';') {
        let cube_counts: HashMap<String, u32> = get_cube_counts(cube_set.to_string());
        for key in cube_counts.keys() {
            if !max_cube_counts.contains_key(key) || max_cube_counts[key] < cube_counts[key] {
                max_cube_counts.insert(key.to_string(), cube_counts[key]);
                continue;
            }
        }
    }
    return max_cube_counts;
}

fn get_power(max_cube_counts: HashMap<String, u32>) -> u32 {
    let mut keys = max_cube_counts.keys();
    let first_key: &String = keys.next().unwrap();
    let mut power: u32 = max_cube_counts[first_key];
    for key in keys {
        power *= max_cube_counts[key];
    }
    return power;
}

fn main() {
    let path: &Path = Path::new("part2.txt");
    let contents: String = fs::read_to_string(path).unwrap();

    let mut sum_powers_of_games: u32 = 0;
    for line in contents.lines() {
        let end_game_id: usize = line.find(':').unwrap();
        let cube_sets: &str = &line[end_game_id + 1..];
        sum_powers_of_games += get_power(get_max_cube_counts_game(cube_sets));
    }
    println!("{}", sum_powers_of_games);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_max_cube_counts_game() -> Result<(), String> {
        let cube_sets: &str = "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green";
        let max_cube_counts = get_max_cube_counts_game(cube_sets);
        assert_eq!(max_cube_counts["red"], 4);
        assert_eq!(max_cube_counts["green"], 2);
        assert_eq!(max_cube_counts["blue"], 6);
        Ok(())
    }

    #[test]
    fn test_get_power() -> Result<(), String> {
        let mut hash_map: HashMap<String, u32> = HashMap::new();
        hash_map.insert("red".to_string(), 2);
        hash_map.insert("green".to_string(), 3);
        hash_map.insert("blue".to_string(), 4);
        assert_eq!(get_power(hash_map), 24);
        Ok(())
    }
}
