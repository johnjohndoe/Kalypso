
package org.kalypso.convert.namodel.test;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import junit.framework.TestCase;

import org.kalypso.convert.namodel.NaModelCalcJob;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.java.io.StreamUtilities;
import org.kalypso.services.calculation.service.CalcJobDataBean;

/**
 * @author doemming
 * 
 * TODO To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Style - Code Templates
 */
public class NaModelCalcJobTest extends TestCase
{

    final String modellGMLResource = "data/namodell.gml";

    final String controlGMLResource = "data/nacontrol.gml";

    public void testRun() throws IOException
    {
        File modellGML = File.createTempFile("NA_MODELL", ".gml");
        File controlGML = File.createTempFile("NA_CONTROL", ".gml");

        StreamUtilities.streamCopy(getClass().getResourceAsStream(
                modellGMLResource), new FileOutputStream(modellGML));
        StreamUtilities.streamCopy(getClass().getResourceAsStream(
                controlGMLResource), new FileOutputStream(controlGML));

        File baseDir = FileUtilities.createNewTempDir("NA_Simulation"); 
        NaModelCalcJob job = new NaModelCalcJob();
        CalcJobDataBean[] beans = new CalcJobDataBean[] {
//                	new CalcJobDataBean("id", "name", "path"),
                new CalcJobDataBean(NaModelCalcJob.MODELL_ID, "Modelldaten", modellGML
                        .getPath()),
                new CalcJobDataBean(NaModelCalcJob.CONTROL_ID, "Steuerdaten", controlGML
                        .getPath()) };
        job.run(baseDir, beans);
    }
}