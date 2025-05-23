export type Post = {
  id: number;
  title: string;
  body: string;
  author: {
    first_name: string;
    last_name: string;
    email: string;
  };
};

export type Place = {
  id: number;
  name: string;
};
