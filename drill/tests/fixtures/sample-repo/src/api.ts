function getUsers(): Promise<User[]> {
    return fetch('/api/users').then(r => r.json());
}

function postUser(user: User): Promise<User> {
    return fetch('/api/users', {
        method: 'POST',
        body: JSON.stringify(user)
    }).then(r => r.json());
}

