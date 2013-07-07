require 'pp'
require 'net/http'
require 'json'

id = ->{ rand(2 ** 64) }

trace = {
  spanId:    id[],
  name:      'test-trace',
  startTime: Time.now.utc.to_i,
  endTime:   Time.now.utc.to_i + 200,
}

spans = [
  {
    traceId:   trace[:spanId],
    parentId:  trace[:spanId],
    span: {
      spanId:    span1_id = id[],
      name:      'span-1',
      startTime: Time.now.utc.to_i,
      endTime: Time.now.utc.to_i + 100,
    }
  },
  {
    traceId:   trace[:spanId],
    parentId:  span1_id,
    span: {
      spanId:    id[],
      name:      'span-2',
      startTime: Time.now.utc.to_i + 4,
      endTime: Time.now.utc.to_i + 100,
    }
  },
  {
    traceId:   trace[:spanId],
    parentId:  span1_id,
    span: {
      spanId:    id[],
      name:      'span-3',
      startTime: Time.now.utc.to_i + 6,
      endTime: Time.now.utc.to_i + 100,
    }
  },
]

$http = Net::HTTP.new('localhost', 8000)

def req(method, path, body = nil)
  req = method.new(path, {'Content-Type' => 'application/json'})
  req.body = body if body
  res = $http.start {|http| http.request(req) }
  puts "#{method.to_s.split("::")[-1].upcase} #{path} #{res.body}"
  res
end

def get(*args);  req Net::HTTP::Get,  *args end
def put(*args);  req Net::HTTP::Put,  *args end
def post(*args); req Net::HTTP::Post, *args end

put "/trace/#{trace[:spanId]}", trace.to_json
get "/trace/#{trace[:spanId]}"

post "/spans", spans.to_json
post "/spans", spans.to_json # Should be idempotent
get "/trace/#{trace[:spanId]}"
