import paramiko
ssh_client = paramiko.SSHClient()
ssh_client.set_missing_host_key_policy(paramiko.AutoAddPolicy())
hostname = '168.188.56.34'
port = 22
username = 'gest1'
password = 'gest1'
ssh_client.connect(hostname, port, username, password)
ssh_session = ssh_client.invoke_shell()
while True:
    command = input("원격 서버에 보낼 명령을 입력하세요 (종료하려면 'exit' 입력): ")
    ssh_session.send(command + "\n")
    output = ssh_session.recv(4096)
    print(output.decode('utf-8'))
    if command.lower() == 'exit':
        break
ssh_session.close()
ssh_client.close()