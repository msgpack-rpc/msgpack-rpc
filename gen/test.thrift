
struct Info {
	string str;
	int num;
}

service Test {
	string Get(string key);
	void Set(list<string> key, string val);
	Info Check();
}

