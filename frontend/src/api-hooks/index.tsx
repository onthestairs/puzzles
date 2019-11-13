/* Generated by restful-react */

import React from "react";
import { Get, GetProps, useGet, UseGetProps } from "restful-react";

export type Omit<T, K extends keyof T> = Pick<T, Exclude<keyof T, K>>;

export interface User {email: string; registration_date: Day; age: number; name: string}

export type Day = string;

export type GetUsersProps = Omit<GetProps<User[], unknown, void>, "path">;


export const GetUsers = (props: GetUsersProps) => (
  <Get<User[], unknown, void>
    path={`/users`}
    {...props}
  />
);

export type UseGetUsersProps = Omit<UseGetProps<User[], void>, "path">;


export const useGetUsers = (props: UseGetUsersProps) => useGet<User[], unknown, void>(`/users`, props);

