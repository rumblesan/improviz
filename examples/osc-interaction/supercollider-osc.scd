Server.default.waitForBoot {
    SynthDef(\sine, {
        var snd;
        snd = SinOsc.ar(\freq.kr(440));
        snd = snd * Env.perc(0.01, 0.3).ar(Done.freeSelf);
        Out.ar(0, snd);
    }).add;

    Server.default.sync;

    i = NetAddr.new("127.0.0.1", 5510);

    Routine({
        10.do {
            var freq;
            freq = exprand(100, 8000);
            i.sendMsg("/vars/freq", freq);
            Synth(\sine, [freq: freq, 8000, amp: 0.1]);
            rrand(0.1, 0.5).yield;
        };
        0.exit;
    }).play;
};
