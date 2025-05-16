FROM mcr.microsoft.com/dotnet/sdk:9.0-bookworm-slim AS builder
WORKDIR /App

# Copy sln and proj files
COPY blazor.dockersln ./blazor.sln
COPY Domain/Domain.fsproj Domain/packages.lock.json ./Domain/
COPY Shell/Shell.csproj Shell/packages.lock.json ./Shell/
# Restore as distinct layers
RUN dotnet restore

# Copy source code
COPY ./Domain ./Domain
COPY ./Shell ./Shell
# Build and publish a release
RUN dotnet publish ./Shell -o out

# Build runtime image
FROM mcr.microsoft.com/dotnet/aspnet:9.0-bookworm-slim AS runner
WORKDIR /App
COPY --from=builder /App/out .

ENTRYPOINT ["dotnet", "Shell.dll"]