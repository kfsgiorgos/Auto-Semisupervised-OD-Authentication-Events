 - Since the components of our methodology do not exist in big data frameworks we have to work on a data sample. Hence, we first select randomly few users in order to extract their corresponding login events from the aut.txt (https://csr.lanl.gov/data/cyber1/)
 - auth.txt dataset is exremely sparse so we have to ensure that at least we select at least a malicipus user. Malicious users have at least one malicious login event. 

The following are the selected users: 

- Normal Users: "U22@DOM1", "ANONYMOUS LOGON@C586", "C480$@DOM1", "U194@DOM1", C830"$@DOM1", "U228@DOM1"
- Malicious users:  "U3005@DOM1", "U66_DOM1", "U737@DOM1", "U748@DOM1", "U293@DOM1"


We extract all the events of the above users on the command line by running the following awk command:

awk -F, '$2==U228@DOM1 {print $1,$2,$3,$4,$5,$6,$7,$8,$9}' auth.txt > give_a_name.txt