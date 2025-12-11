import type { Option$ } from "./build/dev/javascript/gleam_stdlib/gleam/option.d.mts";
import { List, Ok, Result } from "./build/dev/javascript/typescript_declarations/gleam.mjs";
import * as Gleam from "./build/dev/javascript/typescript_declarations/typescript_declarations.mjs"

// Check add and add_alias
const sum: number = Gleam.add_alias(Gleam.add(1, 2), 908);

// Check ID lists constants
const count_of_moderators: number = Gleam.moders.countLength();
const count_of_administrators: number = Gleam.admins.countLength();

// Check add_one closure
const add_one: (_: number) => number = Gleam.add_one()
const two: number = add_one(1)

// Check UserId type alias
const first_moderator: Gleam.UserId = Gleam.moders.toArray()[ 0];
const first_moderator_num: number = first_moderator;
const me: UserID = 84738;
type UserID = Gleam.UserId;

// Check is_divisible_by
const is_five_divisible_by_two: boolean = Gleam.is_divisible_by(5, 2);

// Check extern alert function if we are in browser target
if (typeof window != undefined) {
  Gleam.js_alert("Hello!");
}

// Check generic twice function
const twice_number: number = Gleam.twice(45, add_one);

// Check recursive sum_list
const sum_of_list: number = Gleam.sum_list(List.fromArray([10, 25, 65383, 8910, 1893]), 0);

// Check results
const result_ok: Result<number, number> = new Ok(10);
const is_ok: boolean = Gleam.is_ok_result(result_ok);

// Check name_description tuple
const name: string = Gleam.name_description[0];
const description: string = Gleam.name_description[1];

// Check User and related functions
const user: Gleam.User$ = new Gleam.User("King", 83874, new Gleam.PlainUser());
const guest: Gleam.User$ = new Gleam.Guest();
const user_username: Option$<string> = Gleam.user_name(user);
const guest_username: Option$<string> = Gleam.user_name(guest);
const plain_user_string: string = Gleam.role_string((user as Gleam.User).role);

// Check Either type
const left_either: Gleam.Either$<string, number> = new Gleam.Left("Hello!");
const right_either: Gleam.Either$<string, number> = new Gleam.Right(3747);

// This added since TypeScript will give warnings about unused things otherwise
void [
  sum,
  count_of_administrators,
  count_of_moderators,
  add_one,
  two,
  first_moderator,
  first_moderator_num,
  me,
  is_five_divisible_by_two,
  twice_number,
  sum_of_list,
  result_ok,
  is_ok,
  name,
  description,
  user,
  guest,
  user_username,
  guest_username,
  plain_user_string,
  left_either,
  right_either,
]
