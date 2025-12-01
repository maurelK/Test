# Script de test des endpoints AREAS
# Usage: .\test-areas-endpoints.ps1

Write-Host "🧪 Test des endpoints AREAS" -ForegroundColor Cyan
Write-Host "=======================================" -ForegroundColor Cyan
Write-Host ""

$baseUrl = "http://localhost:8080/api"

# Test 1: Create AREA
Write-Host "1️⃣  Test: POST /areas (Create AREA)" -ForegroundColor Yellow
try {
    $body = @{
        name = "Gmail to Discord"
        trigger_service = "gmail"
        trigger_event = "new_email"
        trigger_config = @{
            from = "boss@company.com"
        }
        action_service = "discord"
        action_type = "send_message"
        action_config = @{
            webhook_url = "https://discord.com/api/webhooks/..."
            message = "New email from boss!"
        }
        is_active = $true
    } | ConvertTo-Json

    $response = Invoke-WebRequest -Uri "$baseUrl/areas" -Method POST -Body $body -ContentType 'applicati