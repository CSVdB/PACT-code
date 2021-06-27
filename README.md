# PACT

Main decisions:
- Backend is written in Haskell
  * Formatter: Ormulu
  * Linter: hlint
  * Package management: Nix
- Frontend is written in Purescript
  * Package management: Nix
- CI: Nix-based (github workflows)
- Deployment is done in Nix on an EC2 AWS instance running NixOS

# AWS EC2 Server instance

EC2 server instance for Ubuntu 20.04 LTS is set up
- Get PEM keypair from Akshat
- Save it in/as: ```/home/$USERNAME/.ssh/keypair01.pem```
- Run the following command to SSH into the EC2 server instance: 
    ```ssh -i /home/$USERNAME/.ssh/keypair01.pem ubuntu@ec2-18-220-148-247.us-east-2.compute.amazonaws.com```
- ...

## Further tasks
- Install AWS CLI: ```sudo apt install awscli```
  - This is a unified tool to check up on all your services directly from your CLI
  - The following info needs to be instantiated:
  ```
  AWS Access Key ID [None]:
  AWS Secret Access Key [None]:
  Default region name [None]:
  Default output format [None]:
  ```
- ...