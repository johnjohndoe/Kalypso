package org.kalypso.convert.namodel;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Writer;
import java.net.URL;

import org.kalypso.java.io.ReaderUtilities;
import org.kalypso.services.calculation.job.impl.AbstractCalcJob;
import org.kalypso.services.calculation.service.CalcJobDataBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;

/**
 * @author doemming
 */
public class NaModelCalJob extends AbstractCalcJob
{

    public void run(File basedir, CalcJobDataBean[] input)
            throws CalcJobServiceException
    {

        try
        {
	        prepareBaseDir(basedir);
            // kopiere template aus resourcen:
            copyTemplates(basedir);
            // generiere ascii-dateien
        } 
        catch (IOException e)
        {
            // TODO throw CalcJobException
            e.printStackTrace();
        }

        // starte berechnung

    }

    private final String[] subDirs = { "hydro.top", "inp.dat", "input",
            "lzsim", "modell", "start" };

    private void prepareBaseDir(File baseDir)
    {
        for (int i = 0; i < subDirs.length; i++)
            (new File(baseDir, subDirs[i])).mkdirs();
    }

    private final String resourceBase = "";

    private final String[] templateResources = { "input.dat/fds" };

private void copyTemplates(File basedir) throws IOException
    {
        for (int i = 0; i < templateResources.length; i++)
        {
            URL url=getClass().getResource(resourceBase+templateResources[i]);
            Writer writer=new FileWriter(new File(basedir,templateResources[i]));
            ReaderUtilities.readerCopy(new InputStreamReader(url.openStream()),writer);
        }
        // TODO Auto-generated method stub

    }}