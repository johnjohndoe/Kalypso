/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.metadoc.impl;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.lang.reflect.InvocationTargetException;

import org.apache.commons.configuration.BaseConfiguration;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.metadoc.IExportableObject;
import org.kalypso.metadoc.KalypsoMetaDocPlugin;
import org.kalypso.metadoc.configuration.IPublishingConfiguration;
import org.kalypso.metadoc.ui.FileSelectionWizardPage;

/**
 * The file-target simply writes the document into a local file.
 * 
 * @author schlienger
 */
public class FileExportTarget extends AbstractExportTarget
{
  /** Must be a File (a directory or a real file) */
  public final static String CONF_FILEEXPORT_FILE = FileExportTarget.class.getName() + ".file";

  /** Extension of the file (must be in the form: .ext ) */
  public final static String CONF_FILEEXPORT_EXTENSION = FileExportTarget.class.getName() + ".extension";

  /**
   * @see org.kalypso.metadoc.IExportTarget#commitDocument(org.kalypso.metadoc.IExportableObject,
   *      org.apache.commons.configuration.Configuration, org.eclipse.core.runtime.IProgressMonitor)
   */
  public IStatus commitDocument( final IExportableObject document, final Configuration conf, final IProgressMonitor monitor ) throws InvocationTargetException
  {
    monitor.beginTask( "Export von " + document.getPreferredDocumentName(), IProgressMonitor.UNKNOWN );

    FileOutputStream stream = null;
    try
    {
      final File file = (File) conf.getProperty( CONF_FILEEXPORT_FILE );
      final File docFile;
      if( file.isDirectory() )
        docFile = new File( file, document.getPreferredDocumentName() );
      else
        docFile = file;

      stream = new FileOutputStream( docFile );
      return document.exportObject( stream, monitor, new BaseConfiguration() );
    }
    catch( final FileNotFoundException e )
    {
      throw new InvocationTargetException( e );
    }
    finally
    {
      IOUtils.closeQuietly( stream );

      monitor.done();
    }
  }

  /**
   * @see org.kalypso.metadoc.IExportTarget#createWizardPages(IPublishingConfiguration)
   */
  public IWizardPage[] createWizardPages( final IPublishingConfiguration configuration )
  {
    final ImageDescriptor imgDesc = AbstractUIPlugin.imageDescriptorFromPlugin( KalypsoMetaDocPlugin.getId(), "icons/newfile_wiz.gif" );
    final FileSelectionWizardPage page = new FileSelectionWizardPage( configuration, "file.export.target.page", "Dateiauswahl", imgDesc, "Datei" );

    return new IWizardPage[] { page };
  }
}
