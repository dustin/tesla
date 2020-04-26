{-# LANGUAGE OverloadedStrings #-}

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Data.Aeson
import           Data.Maybe            (fromJust)
import           Data.Ratio
import           Data.Time.Clock       (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import           Tesla
import           Tesla.Car

sampleVehicleData :: VehicleData
sampleVehicleData = "{\"id\":484265022836,\"user_id\":872475,\"vehicle_id\":842742,\"vin\":\"5YJXCDE2XHF000001\",\"display_name\":\"Bob\",\"option_codes\":\"AD15,MDL3,PBSB,RENA,BT37,ID3W,RF3G,S3PB,DRLH,DV2W,W39B,APF0,COUS,BC3B,CH07,PC30,FC3P,FG31,GLFR,HL31,HM31,IL31,LTPB,MR31,FM3B,RS3H,SA3P,STCP,SC04,SU3C,T3CA,TW00,TM00,UT3P,WR00,AU3P,APH3,AF00,ZCST,MI00,CDM0\",\"color\":null,\"tokens\":[\"token1\",\"token2\"],\"state\":\"online\",\"in_service\":false,\"id_s\":\"484265022836\",\"calendar_enabled\":true,\"api_version\":6,\"backseat_token\":null,\"backseat_token_updated_at\":null,\"charge_state\":{\"battery_heater_on\":false,\"battery_level\":79,\"battery_range\":225.71,\"charge_current_request\":48,\"charge_current_request_max\":48,\"charge_enable_request\":false,\"charge_energy_added\":0.0,\"charge_limit_soc\":80,\"charge_limit_soc_max\":100,\"charge_limit_soc_min\":50,\"charge_limit_soc_std\":90,\"charge_miles_added_ideal\":0.0,\"charge_miles_added_rated\":0.0,\"charge_port_cold_weather_mode\":null,\"charge_port_door_open\":true,\"charge_port_latch\":\"Engaged\",\"charge_rate\":0.0,\"charge_to_max_range\":false,\"charger_actual_current\":0,\"charger_phases\":null,\"charger_pilot_current\":48,\"charger_power\":13,\"charger_voltage\":0,\"charging_state\":\"Stopped\",\"conn_charge_cable\":\"SAE\",\"est_battery_range\":161.61,\"fast_charger_brand\":\"<invalid>\",\"fast_charger_present\":false,\"fast_charger_type\":\"ACSingleWireCAN\",\"ideal_battery_range\":282.78,\"managed_charging_active\":false,\"managed_charging_start_time\":null,\"managed_charging_user_canceled\":false,\"max_range_charge_counter\":0,\"minutes_to_full_charge\":20,\"not_enough_power_to_heat\":false,\"scheduled_charging_pending\":true,\"scheduled_charging_start_time\":1569738600,\"time_to_full_charge\":0.33,\"timestamp\":1569718275490,\"trip_charging\":false,\"usable_battery_level\":79,\"user_charge_enable_request\":null},\"climate_state\":{\"battery_heater\":false,\"battery_heater_no_power\":false,\"climate_keeper_mode\":\"off\",\"defrost_mode\":0,\"driver_temp_setting\":22.0,\"fan_status\":0,\"inside_temp\":22.3,\"is_auto_conditioning_on\":false,\"is_climate_on\":false,\"is_front_defroster_on\":false,\"is_preconditioning\":false,\"is_rear_defroster_on\":false,\"left_temp_direction\":317,\"max_avail_temp\":28.0,\"min_avail_temp\":15.0,\"outside_temp\":15.5,\"passenger_temp_setting\":22.0,\"remote_heater_control_enabled\":false,\"right_temp_direction\":317,\"seat_heater_left\":0,\"seat_heater_rear_center\":0,\"seat_heater_rear_left\":0,\"seat_heater_rear_right\":0,\"seat_heater_right\":0,\"side_mirror_heaters\":false,\"smart_preconditioning\":false,\"steering_wheel_heater\":false,\"timestamp\":1569718275490,\"wiper_blade_heater\":false},\"drive_state\":{\"gps_as_of\":1569718274,\"heading\":125,\"latitude\":34.13245,\"longitude\":-120.823475,\"native_latitude\":34.13245,\"native_location_supported\":1,\"native_longitude\":-120.823475,\"native_type\":\"wgs\",\"power\":0,\"shift_state\":\"P\",\"speed\":null,\"timestamp\":1569718275490},\"gui_settings\":{\"gui_24_hour_time\":true,\"gui_charge_rate_units\":\"kW\",\"gui_distance_units\":\"mi/hr\",\"gui_range_display\":\"Rated\",\"gui_temperature_units\":\"C\",\"show_range_units\":true,\"timestamp\":1569718275490},\"vehicle_config\":{\"can_accept_navigation_requests\":true,\"can_actuate_trunks\":true,\"car_special_type\":\"base\",\"car_type\":\"modelx\",\"charge_port_type\":\"US\",\"eu_vehicle\":false,\"exterior_color\":\"SteelGrey\",\"has_air_suspension\":true,\"has_ludicrous_mode\":false,\"motorized_charge_port\":true,\"plg\":true,\"rear_seat_heaters\":3,\"rear_seat_type\":4,\"rhd\":false,\"roof_color\":\"None\",\"seat_type\":0,\"spoiler_type\":\"Passive\",\"sun_roof_installed\":0,\"third_row_seats\":\"None\",\"timestamp\":1569718275490,\"trim_badging\":\"100d\",\"use_range_badging\":false,\"wheel_type\":\"AeroTurbine20\"},\"vehicle_state\":{\"api_version\":6,\"autopark_state_v2\":\"standby\",\"autopark_style\":\"standard\",\"calendar_supported\":true,\"car_version\":\"2019.32.11 bac8c51\",\"center_display_state\":0,\"df\":0,\"dr\":0,\"fd_window\":0,\"fp_window\":0,\"ft\":0,\"homelink_device_count\":1,\"homelink_nearby\":false,\"is_user_present\":true,\"last_autopark_error\":\"no_error\",\"locked\":true,\"media_state\":{\"remote_control_enabled\":true},\"notifications_supported\":true,\"odometer\":49081.642123,\"parsed_calendar_supported\":true,\"pf\":0,\"pr\":0,\"rd_window\":0,\"remote_start\":false,\"remote_start_enabled\":true,\"remote_start_supported\":true,\"rp_window\":0,\"rt\":32,\"sentry_mode\":false,\"sentry_mode_available\":true,\"smart_summon_available\":true,\"software_update\":{\"download_perc\":0,\"expected_duration_sec\":2700,\"install_perc\":1,\"status\":\"\",\"version\":\"\"},\"speed_limit_mode\":{\"active\":false,\"current_limit_mph\":80.0,\"max_limit_mph\":90,\"min_limit_mph\":50,\"pin_code_set\":true},\"summon_standby_mode_enabled\":true,\"timestamp\":1569718275489,\"valet_mode\":false,\"vehicle_name\":\"Bob\"}}"

sampleProducts :: Value
sampleProducts = fromJust . decode $ "{\"response\":[{\"backseat_token_updated_at\":null,\"backseat_token\":null,\"state\":\"online\",\"display_name\":\"MyCar\",\"option_codes\":\"AD15,MDL3,PBSB,RENA,BT37,ID3W,RF3G,S3PB,DRLH,DV2W,W39B,APF0,COUS,BC3B,CH07,PC30,FC3P,FG31,GLFR,HL31,HM31,IL31,LTPB,MR31,FM3B,RS3H,SA3P,STCP,SC04,SU3C,T3CA,TW00,TM00,UT3P,WR00,AU3P,APH3,AF00,ZCST,MI00,CDM0\",\"color\":null,\"api_version\":7,\"tokens\":[\"tok1\",\"tok2\"],\"id_s\":\"848528\",\"vehicle_id\":87576,\"id\":848528,\"in_service\":false,\"calendar_enabled\":true,\"user_id\":361508,\"vin\":\"SOMEVIN\"},{\"solar_type\":\"pv_panel\",\"energy_site_id\":2848535,\"breaker_alert_enabled\":false,\"solar_power\":0,\"id\":\"SOME_UU_I_D\",\"resource_type\":\"solar\",\"sync_grid_alert_enabled\":false}],\"count\":2}"

testIsUserPresent :: Assertion
testIsUserPresent = assertBool "expected user not present" $ isUserPresent sampleVehicleData

testIsCharging :: Assertion
testIsCharging = assertBool "expected charging" $ isCharging sampleVehicleData

testTimestamp :: Assertion
testTimestamp = assertEqual "timestamp" (posixSecondsToUTCTime . fromRational $ 1569718275489 % 1000) (teslaTS sampleVehicleData)

testDoors :: Assertion
testDoors = assertEqual "doors" (Just [Closed DriverFront,
                                       Closed DriverRear,
                                       Closed PassengerFront,
                                       Closed PassengerRear,
                                       Closed FrontTrunk,
                                       Open RearTrunk]) (doors sampleVehicleData)

testOpenDoors :: Assertion
testOpenDoors =  assertEqual "open" [RearTrunk] (openDoors sampleVehicleData)

testParseProducts :: Assertion
testParseProducts = assertEqual "products" [(ProductCar, "848528", "MyCar"),
                                            (ProductSolar, "SOME_UU_I_D", "pv_panel")]
                    (decodeProducts sampleProducts)

tests :: [TestTree]
tests = [
  testCase "is user present" testIsUserPresent,
  testCase "charging" testIsCharging,
  testCase "timestamp" testTimestamp,
  testCase "doors" testDoors,
  testCase "open doors" testOpenDoors,
  testCase "products parsing" testParseProducts
  ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
