// $Id: TestHAVAL.java,v 1.1 2002/09/05 19:27:06 baford Exp $
//
// $Log: TestHAVAL.java,v $
// Revision 1.1  2002/09/05 19:27:06  baford
// Put together the on-line source code and examples page for the thesis.
//
// Revision 1.1  2002/09/02 20:14:03  baford
// Copied test suite from ICFP paper directory
//
// Revision 1.1  2002/07/25 18:30:31  baford
// Added more files from cryptix32-20001002-r3.2.0/src
// to increase the size of the test suite a bit
//
// Revision 1.5  2000/08/17 11:41:01  edwin
// java.* -> xjava.*
//
// Revision 1.4  1998/01/28 05:40:00  hopwood
// + Major update of test classes.
//
// Revision 1.3  1997/11/22 07:05:40  raif
// *** empty log message ***
//
// Revision 1.2  1997/11/22 05:59:02  iang
// core.util ==> util.core
//
// Revision 1.1  1997/11/07 05:53:26  raif
// *** empty log message ***
//
// Revision 1.2  1997/10/28 00:09:09  raif
// *** empty log message ***
//
// Revision 1.1  1997/10/27 22:50:16  raif
// + use new cryptix.util classes.
// + more compact.
//
// Revision 0.1.0.0  1997/06/??  R. Naffah
// + Original version.
//
/*
 * Copyright (c) 1997 Systemics Ltd
 * on behalf of the Cryptix Development Team. All rights reserved.
 */

package cryptix.test;

import cryptix.util.core.Hex;
import cryptix.util.test.BaseTest;

import java.security.MessageDigest;
import xjava.security.Parameterized;

/**
 * Tests the output of the HAVAL message digest algorithm implementation
 * against certified pre-computed output for a given set of reference input.
 * <p>
 * <b>Copyright</b> &copy; 1997
 * <a href="http://www.systemics.com/">Systemics Ltd</a> on behalf of the
 * <a href="http://www.systemics.com/docs/cryptix/">Cryptix Development Team</a>.
 * <br>All rights reserved.
 * <p>
 * <b>$Revision: 1.1 $</b>
 * @author  Raif S. Naffah
 */
class TestHAVAL
extends BaseTest
{

// Test data
//................................................................................
    
    private static String[][] testData1 = {
    //    PASS, LENGTH, data, md
    //  ......................
         {"3", "128", "",           "C68F39913F901F3DDF44C707357A7D70"}
        ,{"3", "128", "a",          "0CD40739683E15F01CA5DBCEEF4059F1"}
        ,{"3", "128", "HAVAL",      "DC1F3C893D17CC4EDD9AE94AF76A0AF0"}
        ,{"3", "128", "0123456789", "D4BE2164EF387D9F4D46EA8EFB180CF5"}
        ,{"3", "128", "abcdefghijklmnopqrstuvwxyz",
                                    "DC502247FB3EB8376109EDA32D361D82"}
        ,{"3", "128", "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
                                    "DE5EB3F7D9EB08FAE7A07D68E3047EC6"}

        ,{"3", "160", "",           "D353C3AE22A25401D257643836D7231A9A95F953"}
        ,{"3", "160", "a",          "4DA08F514A7275DBC4CECE4A347385983983A830"}
        ,{"3", "160", "HAVAL",      "8822BC6F3E694E73798920C77CE3245120DD8214"}
        ,{"3", "160", "0123456789", "BE68981EB3EBD3F6748B081EE5D4E1818F9BA86C"}
        ,{"3", "160", "abcdefghijklmnopqrstuvwxyz",
                                    "EBA9FA6050F24C07C29D1834A60900EA4E32E61B"}
        ,{"3", "160", "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
                                    "97DC988D97CAAE757BE7523C4E8D4EA63007A4B9"}

        ,{"3", "192", "",           "E9C48D7903EAF2A91C5B350151EFCB175C0FC82DE2289A4E"}
        ,{"3", "192", "a",          "B359C8835647F5697472431C142731FF6E2CDDCACC4F6E08"}
        ,{"3", "192", "HAVAL",      "8DA26DDAB4317B392B22B638998FE65B0FBE4610D345CF89"}
        ,{"3", "192", "0123456789", "DE561F6D818A760D65BDD2823ABE79CDD97E6CFA4021B0C8"}
        ,{"3", "192", "abcdefghijklmnopqrstuvwxyz",
                                    "A25E1456E6863E7D7C74017BB3E098E086AD4BE0580D7056"}
        ,{"3", "192", "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
                                    "DEF6653091E3005B43A61681014A066CD189009D00856EE7"}

        ,{"3", "224", "",           "C5AAE9D47BFFCAAF84A8C6E7CCACD60A0DD1932BE7B1A192B9214B6D"}
        ,{"3", "224", "a",          "731814BA5605C59B673E4CAAE4AD28EEB515B3ABC2B198336794E17B"}
        ,{"3", "224", "HAVAL",      "AD33E0596C575D7175E9F72361CA767C89E46E2609D88E719EE69AAA"}
        ,{"3", "224", "0123456789", "EE345C97A58190BF0F38BF7CE890231AA5FCF9862BF8E7BEBBF76789"}
        ,{"3", "224", "abcdefghijklmnopqrstuvwxyz",
                                    "06AE38EBC43DB58BD6B1D477C7B4E01B85A1E7B19B0BD088E33B58D1"}
        ,{"3", "224", "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
                                    "939F7ED7801C1CE4B32BC74A4056EEE6081C999ED246907ADBA880A7"}

        ,{"3", "256", "",           "4F6938531F0BC8991F62DA7BBD6F7DE3FAD44562B8C6F4EBF146D5B4E46F7C17"}
        ,{"3", "256", "a",          "47C838FBB4081D9525A0FF9B1E2C05A98F625714E72DB289010374E27DB021D8"}
        ,{"3", "256", "HAVAL",      "91850C6487C9829E791FC5B58E98E372F3063256BB7D313A93F1F83B426AEDCC"}
        ,{"3", "256", "0123456789", "63238D99C02BE18C3C5DB7CCE8432F51329012C228CCC17EF048A5D0FD22D4AE"}
        ,{"3", "256", "abcdefghijklmnopqrstuvwxyz",
                                    "72FAD4BDE1DA8C8332FB60561A780E7F504F21547B98686824FC33FC796AFA76"}
        ,{"3", "256", "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
                                    "899397D96489281E9E76D5E65ABAB751F312E06C06C07C9C1D42ABD31BB6A404"}
    };
    private static String[][] testData2 = {
         {"4", "128", "",           "EE6BBF4D6A46A679B3A856C88538BB98"}
        ,{"4", "128", "a",          "5CD07F03330C3B5020B29BA75911E17D"}
        ,{"4", "128", "HAVAL",      "958195D3DAC591030EAA0292A37A0CF2"}
        ,{"4", "128", "0123456789", "2215D3702A80025C858062C53D76CBE5"}
        ,{"4", "128", "abcdefghijklmnopqrstuvwxyz",
                                    "B2A73B99775FFB17CD8781B85EC66221"}
        ,{"4", "128", "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
                                    "CAD57C0563BDA208D66BB89EB922E2A2"}

        ,{"4", "160", "",           "1D33AAE1BE4146DBAACA0B6E70D7A11F10801525"}
        ,{"4", "160", "a",          "E0A5BE29627332034D4DD8A910A1A0E6FE04084D"}
        ,{"4", "160", "HAVAL",      "221BA4DD206172F12C2EBA3295FDE08D25B2F982"}
        ,{"4", "160", "0123456789", "E387C743D14DF304CE5C7A552F4C19CA9B8E741C"}
        ,{"4", "160", "abcdefghijklmnopqrstuvwxyz",
                                    "1C7884AF86D11AC120FE5DF75CEE792D2DFA48EF"}
        ,{"4", "160", "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
                                    "148334AAD24B658BDC946C521CDD2B1256608C7B"}

        ,{"4", "192", "",           "4A8372945AFA55C7DEAD800311272523CA19D42EA47B72DA"}
        ,{"4", "192", "a",          "856C19F86214EA9A8A2F0C4B758B973CCE72A2D8FF55505C"}
        ,{"4", "192", "HAVAL",      "0C1396D7772689C46773F3DAACA4EFA982ADBFB2F1467EEA"}
        ,{"4", "192", "0123456789", "C3A5420BB9D7D82A168F6624E954AAA9CDC69FB0F67D785E"}
        ,{"4", "192", "abcdefghijklmnopqrstuvwxyz",
                                    "2E2E581D725E799FDA1948C75E85A28CFE1CF0C6324A1ADA"}
        ,{"4", "192", "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
                                    "E5C9F81AE0B31FC8780FC37CB63BB4EC96496F79A9B58344"}

        ,{"4", "224", "",           "3E56243275B3B81561750550E36FCD676AD2F5DD9E15F2E89E6ED78E"}
        ,{"4", "224", "a",          "742F1DBEEAF17F74960558B44F08AA98BDC7D967E6C0AB8F799B3AC1"}
        ,{"4", "224", "HAVAL",      "85538FFC06F3B1C693C792C49175639666F1DDE227DA8BD000C1E6B4"}
        ,{"4", "224", "0123456789", "BEBD7816F09BAEECF8903B1B9BC672D9FA428E462BA699F814841529"}
        ,{"4", "224", "abcdefghijklmnopqrstuvwxyz",
                                    "A0AC696CDB2030FA67F6CC1D14613B1962A7B69B4378A9A1B9738796"}
        ,{"4", "224", "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
                                    "3E63C95727E0CD85D42034191314401E42AB9063A94772647E3E8E0F"}

        ,{"4", "256", "",           "C92B2E23091E80E375DADCE26982482D197B1A2521BE82DA819F8CA2C579B99B"}
        ,{"4", "256", "a",          "E686D2394A49B44D306ECE295CF9021553221DB132B36CC0FF5B593D39295899"}
        ,{"4", "256", "HAVAL",      "E20643CFA66F5BE2145D13ED09C2FF622B3F0DA426A693FA3B3E529CA89E0D3C"}
        ,{"4", "256", "0123456789", "ACE5D6E5B155F7C9159F6280327B07CBD4FF54143DC333F0582E9BCEB895C05D"}
        ,{"4", "256", "abcdefghijklmnopqrstuvwxyz",
                                    "124F6EB645DC407637F8F719CC31250089C89903BF1DB8FAC21EA4614DF4E99A"}
        ,{"4", "256", "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
                                    "46A3A1DFE867EDE652425CCD7FE8006537EAD26372251686BEA286DA152DC35A"}
    };
    private static String[][] testData3 = {
         {"5", "128", "",           "184B8482A0C050DCA54B59C7F05BF5DD"}
        ,{"5", "128", "a",          "F23FBE704BE8494BFA7A7FB4F8AB09E5"}
        ,{"5", "128", "HAVAL",      "C97990F4FCC8FBA76AF935C405995355"}
        ,{"5", "128", "0123456789", "466FDCD81C3477CAC6A31FFA1C999CA8"}
        ,{"5", "128", "abcdefghijklmnopqrstuvwxyz",
                                    "0EFFF71D7D14344CBA1F4B25F924A693"}
        ,{"5", "128", "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
                                    "4B27D04DDB516BDCDFEB96EB8C7C8E90"}

        ,{"5", "160", "",           "255158CFC1EED1A7BE7C55DDD64D9790415B933B"}
        ,{"5", "160", "a",          "F5147DF7ABC5E3C81B031268927C2B5761B5A2B5"}
        ,{"5", "160", "HAVAL",      "7730CA184CEA2272E88571A7D533E035F33B1096"}
        ,{"5", "160", "0123456789", "41CC7C1267E88CEF0BB93697D0B6C8AFE59061E6"}
        ,{"5", "160", "abcdefghijklmnopqrstuvwxyz",
                                    "917836A9D27EED42D406F6002E7D11A0F87C404C"}
        ,{"5", "160", "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
                                    "6DDBDE98EA1C4F8C7F360FB9163C7C952680AA70"}

        ,{"5", "192", "",           "4839D0626F95935E17EE2FC4509387BBE2CC46CB382FFE85"}
        ,{"5", "192", "a",          "5FFA3B3548A6E2CFC06B7908CEB5263595DF67CF9C4B9341"}
        ,{"5", "192", "HAVAL",      "794A896D1780B76E2767CC4011BAD8885D5CE6BD835A71B8"}
        ,{"5", "192", "0123456789", "A0B635746E6CFFFFD4B4A503620FEF1040C6C0C5C326476E"}
        ,{"5", "192", "abcdefghijklmnopqrstuvwxyz",
                                    "85F1F1C0ECA04330CF2DE5C8C83CF85A611B696F793284DE"}
        ,{"5", "192", "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
                                    "D651C8AC45C9050810D9FD64FC919909900C4664BE0336D0"}

        ,{"5", "224", "",           "4A0513C032754F5582A758D35917AC9ADF3854219B39E3AC77D1837E"}
        ,{"5", "224", "a",          "67B3CB8D4068E3641FA4F156E03B52978B421947328BFB9168C7655D"}
        ,{"5", "224", "HAVAL",      "9D7AE77B8C5C8C1C0BA854EBE3B2673C4163CFD304AD7CD527CE0C82"}
        ,{"5", "224", "0123456789", "59836D19269135BC815F37B2AEB15F894B5435F2C698D57716760F2B"}
        ,{"5", "224", "abcdefghijklmnopqrstuvwxyz",
                                    "1B360ACFF7806502B5D40C71D237CC0C40343D2000AE2F65CF487C94"}
        ,{"5", "224", "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
                                    "180AED7F988266016719F60148BA2C9B4F5EC3B9758960FC735DF274"}

        ,{"5", "256", "",           "BE417BB4DD5CFB76C7126F4F8EEB1553A449039307B1A3CD451DBFDC0FBBE330"}
        ,{"5", "256", "a",          "DE8FD5EE72A5E4265AF0A756F4E1A1F65C9B2B2F47CF17ECF0D1B88679A3E22F"}
        ,{"5", "256", "HAVAL",      "153D2C81CD3C24249AB7CD476934287AF845AF37F53F51F5C7E2BE99BA28443F"}
        ,{"5", "256", "0123456789", "357E2032774ABBF5F04D5F1DEC665112EA03B23E6E00425D0DF75EA155813126"}
        ,{"5", "256", "abcdefghijklmnopqrstuvwxyz",
                                    "C9C7D8AFA159FD9E965CB83FF5EE6F58AEDA352C0EFF005548153A61551C38EE"}
        ,{"5", "256", "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
                                    "B45CB6E62F2B1320E4F8F1B0B273D45ADD47C321FD23999DCF403AC37636D963"}
    };


// Test methods
//................................................................................

    public static void main(String[] args) {
        new TestHAVAL().commandline(args);
    }

    protected void engineTest() throws Exception {
        setExpectedPasses(90);

        MessageDigest alg = MessageDigest.getInstance("HAVAL", "Cryptix");
        test(alg, testData1);
        out.println("--------------------------------------------");
        test(alg, testData2);
        out.println("--------------------------------------------");
        test(alg, testData3);
        out.println("--------------------------------------------");
    }

    private void test (MessageDigest alg, String[][] data) throws Exception {
        byte[] x;
        String a;
        for (int i = 0; i < data.length; i++) {
            ((Parameterized) alg).setParameter("passes", Integer.valueOf(data[i][0]));
            ((Parameterized) alg).setParameter("bitLength", Integer.valueOf(data[i][1]));
            if (i != 6)
                x = alg.digest(data[i][2].getBytes());
            else {                // try the engineUpdate(byte) method
                for (int j = 0; j < data[i][2].length(); j++)
                    alg.update((byte)(data[i][2].charAt(j)));
                x = alg.digest();
            }
            a = Hex.toString(x);
            out.println("Passes:" + data[i][0] + ", bit length: " + data[i][1] +
                        " data: '" + data[i][2] + "'");
            out.println("  computed md: " + a);
            out.println(" certified md: " + data[i][3]);
            out.print(" ********* HAVAL(" + data[i][0] + ", " + data[i][1] + ") ");
            passIf(a.equals(data[i][3]), "HAVAL #" + (i+1));
        }
    }
}
