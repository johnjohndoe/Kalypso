package org.kalypso.model.wspm.sobek.calculation.job.worker;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ConsoleHelper;
import org.kalypso.contribs.java.io.MyPrintStream;
import org.kalypso.model.wspm.sobek.calculation.job.i18n.Messages;

public class SimulationBaseWorker implements ICoreRunnableWithProgress
{

  private final MyPrintStream m_outputStream;

  private final File m_tmpDir;

  private final IPath m_sobekInstallationDir;

  public SimulationBaseWorker( final File tmpDir, final IPath sobekInstallationDir, final MyPrintStream outputStream )
  {
    m_tmpDir = tmpDir;
    m_sobekInstallationDir = sobekInstallationDir;
    m_outputStream = outputStream;
  }

  private void extractCalculationCore(  ) throws IOException
  {
    InputStream zipStream = null;
    zipStream = getClass().getResourceAsStream( "/org/kalypso/model/wspm/sobek/calculation/job/resources/calculationCore.zip" ); //$NON-NLS-1$
    if( zipStream != null )
    {
      ZipUtilities.unzipApache( zipStream, m_tmpDir, true, "IBM850" ); //$NON-NLS-1$
      zipStream.close();
    }
  }

  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    try
    { 
      ConsoleHelper.writeLine( m_outputStream, String.format( Messages.SimulationBaseWorker_0)); //$NON-NLS-1$

      /* extract computation base directories */
      extractCalculationCore(  );

      ConsoleHelper.writeLine( m_outputStream, String.format( Messages.SimulationBaseWorker_1)); //$NON-NLS-1$
      /* adjusting sobek runtime scripts */
      adjustScripts();
      
      
      return Status.OK_STATUS;
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new CoreException( StatusUtilities.createErrorStatus( "Simulation base setup failed" ) ); //$NON-NLS-1$
    }
  }

  private void adjustScripts( ) throws CoreException
  {
    final File pi2Sobek = new File(m_tmpDir,"Sobek-IDSS/batch/1_PI2sobek.bat" ); //$NON-NLS-1$
    if (!pi2Sobek.exists())
      throw new CoreException(StatusUtilities.createErrorStatus( "Sobek-IDSS/batch/1_PI2sobek.bat file is missing." )); //$NON-NLS-1$
    
    final File sobekRiverOmi = new File(m_tmpDir, "Sobek-IDSS/data/iDSS2OpenMI/sobek_river.omi"); //$NON-NLS-1$
    if (!sobekRiverOmi.exists())
      throw new CoreException(StatusUtilities.createErrorStatus( "Sobek-IDSS/data/iDSS2OpenMI/sobek_river.omi file is missing." )); //$NON-NLS-1$
    
    final String sobekDirectory = m_sobekInstallationDir.toOSString().replace( ":", ":\\" ); //$NON-NLS-1$ //$NON-NLS-2$
    
    final Map<String, String> replacements = new HashMap<String, String>();
    replacements.put( "%SOBEKROOT%", sobekDirectory ); //$NON-NLS-1$
    
    new StringFileReplacer(pi2Sobek, replacements).execute( new NullProgressMonitor() );
    new StringFileReplacer(sobekRiverOmi, replacements).execute( new NullProgressMonitor() );
  }

}
