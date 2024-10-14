use std::fmt;
use std::fmt::Write;

#[derive(PartialEq, PartialOrd, Debug)]
pub struct Clock {
    hours: i32,
    minutes: i32,
}

const DAILY_MINUTES: i32 = 24 * 60;
const I32_CLOCK_MAX_MINUTES: i32 = i32::MAX - i32::MAX % DAILY_MINUTES;

impl Clock {
    pub fn new(hours: i32, minutes: i32) -> Self {
        Clock {hours: 0, minutes: 0}.add_minutes(hours * 60 + minutes)
    }

    pub fn add_minutes(&self, minutes: i32) -> Self {
        let total_minutes = self.hours * 60 + self.minutes + minutes;
        let positive_minutes =
            if total_minutes < 0 {
                I32_CLOCK_MAX_MINUTES + total_minutes
            } else {
                total_minutes
            };
        let day_minutes = positive_minutes % DAILY_MINUTES;

        Clock { hours: day_minutes / 60, minutes: day_minutes % 60 }
    }
}

impl fmt::Display for Clock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:02}:{:02}", self.hours, self.minutes)
    }
}

fn should_add_zero(num: i32) -> bool {
    num / 10 > 0
}
