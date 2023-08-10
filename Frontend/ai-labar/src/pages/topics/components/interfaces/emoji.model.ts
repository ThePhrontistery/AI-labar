export interface Emoji {
  id: number;
  icon: string;
  name: string;
  selected: boolean;
}
export interface IUser {
  name: string;
  checked: boolean;
  hidden: boolean;
}
export interface IResult {
  option: string;
  votes: number;
  image: string;
}
export interface IEmojiResult {
  option: string;
  votes: number;
  emoji: Emoji;
}
export interface IResultImage {
  option: string;
  votes: number;
  image: string;
}

