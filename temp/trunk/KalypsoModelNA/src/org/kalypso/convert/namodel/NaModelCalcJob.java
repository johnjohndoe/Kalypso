package org.kalypso.convert.namodel;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.Writer;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.deegree.model.feature.GMLWorkspace;
import org.kalypso.java.io.ReaderUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.services.calculation.job.impl.AbstractCalcJob;
import org.kalypso.services.calculation.job.impl.CalcJobHelper;
import org.kalypso.services.calculation.service.CalcJobDataBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;

/**
 * @author doemming
 */
public class NaModelCalcJob extends AbstractCalcJob
{
    // IDs
    public final static String MODELL_ID = "Modell";

    public final static String CONTROL_ID = "Control";

    // subdirectories to create
    private final static String[] subDirs = { "lzsim", "start", "zufluss",
            "hydro.top", "inp.dat", "klima.dat", "out_we.nat" };

    // resourcebase for static files used in calculation
    private final String m_resourceBase = "template/";

    // static resources under resourcebase

    private final String EXE_FILE = "start/kalypso.sh";

    private final String TEMPLATE_CONF_FILE = "misc/resourceFile.conf";

    public void run(File basedir, CalcJobDataBean[] input)
            throws CalcJobServiceException
    {
        if (!basedir.exists())
            basedir.mkdirs();
        try
        {
            prepareBaseDir(basedir);
            // kopiere template aus resourcen:
            copyTemplates(basedir);
            // generiere ascii-dateien
            generateASCII(basedir, input);
            // starte berechnung
//            startCalculation(basedir);
            // ergebnisse aufbereiten

        } catch (Exception e)
        {
            throw new CalcJobServiceException(
                    "Simulation konnte nicht durchgefuehrt werden", e);
        }
    }

    private void generateASCII(File basedir, CalcJobDataBean[] beans)
            throws Exception
    {
        final NAConfiguration conf = new NAConfiguration(basedir);
        final CalcJobDataBean modellBean = CalcJobHelper.getBeanForId(
                MODELL_ID, beans);
        final URL modellURL = new File(modellBean.getPath()).toURL();
        final GMLWorkspace modellWorkspace = GmlSerializer.createGMLWorkspace(
                modellURL, conf.getSchemaURL());
        Main.featureToAscii(conf, modellWorkspace);

        final CalcJobDataBean controlBean = CalcJobHelper.getBeanForId(CONTROL_ID,beans);
        final URL controlURL = new File(controlBean.getPath()).toURL();
        final GMLWorkspace controlWorkspace = GmlSerializer.createGMLWorkspace(
                controlURL, conf.getControlSchemaURL());
        KonfigWrite.featureToASCII(basedir, controlWorkspace, modellWorkspace);
        Main.featureToAscii(conf, modellWorkspace);
        
        //        final CalcJobDataBean controlBean =
        // CalcJobHelper.getBeanForId(CONTROL_ID,beans);

    }

    private void prepareBaseDir(File baseDir)
    {
        for (int i = 0; i < subDirs.length; i++)
            (new File(baseDir, subDirs[i])).mkdirs();
    }

    private void copyTemplates(File basedir) throws IOException
    {
        String[] templateResources = getTemplateResources();
        for (int i = 0; i < templateResources.length; i++)
        {
            final File destFile = new File(basedir, templateResources[i]);
            final String resource = m_resourceBase + templateResources[i];
            System.out.print("resource: " + resource);
            if (!destFile.exists())
            {
                final URL url = getClass().getResource(resource);
                final Writer writer = new FileWriter(destFile);
                ReaderUtilities.readerCopy(new InputStreamReader(url
                        .openStream()), writer);
                System.out.println(" ...copied");
            } else
                System.out.println(" exists");
        }
    }

    private String[] getTemplateResources() throws IOException
    {
        List result = new ArrayList();
        LineNumberReader reader = new LineNumberReader(new InputStreamReader(
                getClass().getResourceAsStream(TEMPLATE_CONF_FILE)));
        String line = null;
        try
        {
            while ((line = reader.readLine()) != null)
                if (!line.startsWith("#"))
                    result.add(line);
        } catch (IOException e)
        {
            throw e;
        } finally
        {
            reader.close();
        }
        return (String[]) result.toArray(new String[result.size()]);
    }

    private void startCalculation(final File basedir)
            throws CalcJobServiceException
    {
        InputStreamReader inStream = null;
        InputStreamReader errStream = null;

        try
        {
            final File exeFile = new File(basedir, EXE_FILE);
            final File exeDir = exeFile.getParentFile();
            final String commandString = exeFile.getAbsolutePath();

            final Process process = Runtime.getRuntime().exec(commandString,
                    null, exeDir);

            inStream = new InputStreamReader(process.getInputStream());
            errStream = new InputStreamReader(process.getErrorStream());
            while (true)
            {
                ReaderUtilities.dumpAllAvailable(inStream);
                ReaderUtilities.dumpAllAvailable(errStream);

                try
                {
                    process.exitValue();
                    return;
                } catch (IllegalThreadStateException e)
                {
                    // noch nicht fertig
                }

                if (isCanceled())
                {
                    process.destroy();
                    return;
                }
                Thread.sleep(100);
            }
        } catch (final IOException e)
        {
            e.printStackTrace();
            throw new CalcJobServiceException("Fehler beim Ausf?hren", e);
        } catch (final InterruptedException e)
        {
            e.printStackTrace();
            throw new CalcJobServiceException("Fehler beim Ausf?hren", e);
        } finally
        {
            try
            {
                if (inStream != null)
                    inStream.close();

                if (errStream != null)
                    errStream.close();
            } catch (final IOException e1)
            {
                e1.printStackTrace();
            }
        }
    }

}