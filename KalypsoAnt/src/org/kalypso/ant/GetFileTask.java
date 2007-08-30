/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ant;

import java.awt.Frame;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Level;

import javax.swing.JFileChooser;

import org.apache.commons.io.FileUtils;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;
import org.kalypso.contribs.java.swing.filechooser.RegexFileFilter;
import org.kalypso.contribs.java.util.logging.ILogger;
import org.kalypso.contribs.java.util.logging.LoggerUtilities;

/**
 * Loads (ant-)properties from a gml.
 * 
 * <pre>
 *  <kalypso.getFileTask sourceDir="C:\MyDataDir" targetFile="C:\MyFiles\data.file" resultProperty="getFileResult"/>
 * </pre>
 * 
 * @author Gernot Belger
 */
public class GetFileTask extends Task
{
  public final static class FileFilter
  {
    private String m_description;

    private String m_pattern;

    public String getDescription()
    {
      return m_description;
    }

    public void setDescription( String description )
    {
      m_description = description;
    }

    public String getPattern()
    {
      return m_pattern;
    }

    public void setPattern( String pattern )
    {
      m_pattern = pattern;
    }
  }

  private final PropertyAdder m_propertyAdder = new PropertyAdder( this );

  private final List m_filters = new LinkedList();

  /** Initial directory for the file open dialog. */
  private File m_sourceDir;

  /** Destination where to copy the coosen file */
  private File m_targetFile;

  private String m_resultProperty;

  public File getTargetFile()
  {
    return m_targetFile;
  }

  public void setTargetFile( final File targetFile )
  {
    m_targetFile = targetFile;
  }

  public File getSourceDir()
  {
    return m_sourceDir;
  }

  public void setSourceDir( final File sourceDir )
  {
    m_sourceDir = sourceDir;
  }

  public String getResultProperty()
  {
    return m_resultProperty;
  }

  public void setResultProperty( String resultProperty )
  {
    m_resultProperty = resultProperty;
  }

  public FileFilter createFileFilter()
  {
    final FileFilter ff = new FileFilter();
    m_filters.add( ff );
    return ff;
  }

  /**
   * @see org.apache.tools.ant.Task#execute()
   */
  public void execute() throws BuildException
  {
    final Project antProject = getProject();
    final ILogger logger = new ILogger()
    {
      /**
       * @see org.kalypso.contribs.java.util.logging.ILogger#log(java.util.logging.Level, int, java.lang.String)
       */
      public void log( final Level level, final int msgCode, final String message )
      {
        final String outString = LoggerUtilities.formatLogStylish( level, msgCode, message );
        if( antProject == null )
          System.out.println( outString );
        else
          antProject.log( outString );
      }
    };

    final String taskDesk = getDescription();
    final String msgTitle = taskDesk == null ? "Datei öffnen" : taskDesk;
    if( taskDesk != null )
      logger.log( Level.INFO, LoggerUtilities.CODE_NEW_MSGBOX, msgTitle );

    // validieren
    if( m_targetFile == null )
    {
      logger.log( Level.SEVERE, LoggerUtilities.CODE_SHOW_MSGBOX, "Property 'targetFile' must be set." );
      return;
    }

    //  REMARK: it seems that is impossible to retrieve a shell within the ant-thread
    //  So we are using swing instead...
    //  HACK: we need a parent-frame, so we use the very first available... is this always ok?
    final JFileChooser fileChooser = new JFileChooser();
    if( m_sourceDir != null )
      fileChooser.setCurrentDirectory( m_sourceDir );
    fileChooser.setDialogTitle( msgTitle );
    fileChooser.setDialogType( JFileChooser.OPEN_DIALOG );

    for( final Iterator iter = m_filters.iterator(); iter.hasNext(); )
    {
      final FileFilter ff = (FileFilter)iter.next();
      fileChooser.addChoosableFileFilter( new RegexFileFilter( ff.getDescription(), ff.getPattern() ) );
    }

//    Display display = PlatformUI.getWorkbench().getDisplay();
//    Shell shell = display.getActiveShell();
//      System.out.println( "Shell: " + shell );
    
    if( m_filters.size() > 0 )
      fileChooser.setFileFilter( fileChooser.getChoosableFileFilters()[1] );

    // HACK: normally we could use null, as parent component, but in that case the dialog appears as a spearate
    // application
    // in windows task menu and it is also possible that the dialog is behind kalypso.
    // When using the first of all available frames, the dialog gets part of the kalypso-application, it is only
    // possible to hide it behind the progress-bar window.
    // REMARK: this only works, if a map is present, as the map resides inside a embedded frame (see SWT-AWT), so we
    // really are using one
    // of the currently open maps as the parent for this dialog...
    // QUESTION: is it somehow possible to a) get the handle to the progress dialog window, and b) to use this somehow
    // as the parnt component?
    final Frame[] frames = Frame.getFrames();
    final Frame frame = frames[0];
//    frame.toFront();

    frame.addMouseListener( new MouseAdapter()
    {
      /**
       * @see java.awt.event.MouseAdapter#mouseEntered(java.awt.event.MouseEvent)
       */
      public void mouseEntered( MouseEvent e )
      {
        System.out.println( "Hallo" );
        frame.toFront();
      }
    } );

    if( fileChooser.showOpenDialog( frame ) != JFileChooser.APPROVE_OPTION )
    {
      logger.log( Level.INFO, LoggerUtilities.CODE_SHOW_MSGBOX,
          "Abbruch durch den Benutzer, der Gebietsniederschlag wurde nicht erneut berechnet." );
      return;
    }

    final File externalFile = fileChooser.getSelectedFile();
    //    final String resultDir = fileDialog.getDirectory();
    //    final String resultFile = fileDialog.getFile();

    // 'Datei öffnen'-Dialog anzeigen
    //    final Display display = PlatformUI.getWorkbench().getDisplay();
    //    final Shell shell = new GetShellFromDisplay( display ).getShell();
    //
    //    final FileDialog dialog = new FileDialog( shell, SWT.OPEN );
    //    if( m_sourceDir != null )
    //      dialog.setFilterPath( m_sourceDir.getAbsolutePath() );
    //    final String resultPath = dialog.open();

    //    if( resultFile == null )
    //    {
    //      logger.log( Level.INFO, LoggerUtilities.CODE_SHOW_MSGBOX,
    //          "Abbruch durch den Benutzer, der Gebietsniederschlag wurde nicht erneut berechnet." );
    //
    //      return;
    //    }

    //    final File externalFile = new File( resultDir, resultFile );

    logger.log( Level.INFO, LoggerUtilities.CODE_SHOW_DETAILS, "Gewählte Datei: " + externalFile.getAbsolutePath() );
    logger.log( Level.INFO, LoggerUtilities.CODE_SHOW_DETAILS, "Datei wird kopiert nach: " + m_targetFile );

    // Datei kopieren
    try
    {
      FileUtils.copyFile( externalFile, m_targetFile );
    }
    catch( final IOException e )
    {
      logger.log( Level.SEVERE, LoggerUtilities.CODE_SHOW_MSGBOX, "Fehler beim Kopieren der Datei: "
          + e.getLocalizedMessage() );
      return;
    }

    // evtl. ergebnis property setzen
    if( m_resultProperty != null )
      m_propertyAdder.addProperty( m_resultProperty, "true", null );
  }
}
