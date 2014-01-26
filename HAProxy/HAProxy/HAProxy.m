(*
Copyright (c) 2007 by Matthew D. Langston. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License"); you
may not use this software or any of it's contents except in
compliance with the License. The full text of the license is in the
file LICENSE.txt in the top-level directory of this project, or you
may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0
*)
BeginPackage["HAProxy`",
	{
		"Utilities`UnixTime`"
	}
]

HAProxyRawData::usage         = "";
HAProxyHeader::usage          = "";
HAProxyData::usage            = "";

HAProxyURL::usage             = "";
HAProxyUsername::usage        = "";
HAProxyPassword::usage        = "";

DefaultHAProxyURL::usage      = "";
DefaultHAProxyUsername::usage = "";
DefaultHAProxyPassword::usage = "";

Begin["`Private`"]

DefaultHAProxyURL      = "http://vagrantx.cloud.appcelerator.com/-haproxy;csv";
DefaultHAProxyUsername = "admin";
DefaultHAProxyPassword = "adm!n00$$";

Options[HAProxyRawData] = {
	HAProxyURL      :> DefaultHAProxyURL,
	HAProxyUsername :> DefaultHAProxyUsername,
	HAProxyPassword :> DefaultHAProxyPassword
};

HAProxyRawData[opts:OptionsPattern[]] := Module[{url, username, password, result},
	url      = OptionValue[HAProxyURL];
	username = OptionValue[HAProxyUsername];
	password = OptionValue[HAProxyPassword];
	result   = URLFetch[url, Username -> username, Password -> password];
	ImportString[result, "CSV"]
];

Options[HAProxyHeader] = {
	HAProxyURL      :> DefaultHAProxyURL,
	HAProxyUsername :> DefaultHAProxyUsername,
	HAProxyPassword :> DefaultHAProxyPassword
};


HAProxyHeader[opts:OptionsPattern[]] := Module[{rawData},
	rawData = HAProxyRawData[FilterRules[{opts}, Options[HAProxyRawData]]];
	Join[{"timestamp"}, rawData[[1]]]
];

Options[HAProxyData] = {
	HAProxyURL      :> DefaultHAProxyURL,
	HAProxyUsername :> DefaultHAProxyUsername,
	HAProxyPassword :> DefaultHAProxyPassword
};


HAProxyData[opts:OptionsPattern[]] := Module[{rawData, timestamp},
	rawData = HAProxyRawData[FilterRules[{opts}, Options[HAProxyRawData]]];
	timestamp = CurrentTimeMillis[];
	Map[Join[{timestamp}, #]&, rawData[[2;;]]]
];

End[]

EndPackage[]
