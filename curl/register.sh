curl -X POST http://localhost:8000/register  --data-binary '{"entry":{"keys":{"application":"poller","type":"dev"},"data":"what ever you think the data should be","lease":60000}}' -v -H "Content-Type: application/json" 
