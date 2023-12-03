use std::fs;
use std::path::Path;

fn is_cube_set_possible(cube_set: String) -> bool {
    for cube_string in cube_set.split(',') {
        let trimmed_start_cube_string: &str = cube_string.trim();
        let count_color: Vec<&str> = trimmed_start_cube_string.split_whitespace().collect();
        let cube_count: u32 = count_color[0].parse::<u32>().unwrap();
        let max: u32;
        if count_color[1] == "red" {
            max = 12;
        }
        else if count_color[1] == "green" {
            max = 13;
        }
        else {
            max = 14;
        }
        if cube_count > max {
            return false;
        }
    }
    return true;
}

fn is_game_possible(cube_sets: &str) -> bool {
    for cube_set in cube_sets.split(';') {
        if !is_cube_set_possible(cube_set.to_string()) {
            return false;
        }
    }
    return true;
}

fn main() {
    let path: &Path = Path::new("part1.txt");
    let contents: String = fs::read_to_string(path).unwrap();

    let mut sum_possible_games: u32 = 0;
    for (line_number, line) in contents.lines().enumerate() {
        let end_game_id: usize = line.find(':').unwrap();
        let cube_sets: &str = &line[end_game_id + 1..];
        if is_game_possible(cube_sets) {
            sum_possible_games += line_number as u32 + 1;
        }
    }
    println!("{}", sum_possible_games);
}
