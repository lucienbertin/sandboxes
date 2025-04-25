FROM mcr.microsoft.com/dotnet/sdk:9.0 AS builder
WORKDIR /App

# Copy everything
COPY . ./
# Restore as distinct layers
RUN dotnet restore
# Build and publish a release
RUN dotnet publish ./Shell -o out

# Build runtime image
FROM mcr.microsoft.com/dotnet/aspnet:9.0 AS runner
WORKDIR /App
COPY --from=builder /App/out .

ENTRYPOINT ["dotnet", "Shell.dll"]