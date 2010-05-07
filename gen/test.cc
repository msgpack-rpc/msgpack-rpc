#include "test.h"

void Test::server::Get(msgpack::rpc::request::type<std::string> req, std::string key)
{
	std::cout << "Get: " << key << std::endl;
	req.result(key);
}

void Test::server::Set(msgpack::rpc::request::type<void> req, std::vector<std::string> key, std::string val)
{
	std::cout << "Set: " << val << std::endl;
	req.result();
}

void Test::server::Check(msgpack::rpc::request::type<Info> req)
{
	std::cout << "Check: " << std::endl;
	Info i = {"str", 1};
	req.result(i);
}

int main(void)
{
	Test::server t;
	t.instance.listen("0.0.0.0", 9090);
	t.instance.start(4);
	
	Test::client c("127.0.0.1", 9090);
	c.Get("test");
	c.Get("test");

	c.Get_async("test").get();
}

