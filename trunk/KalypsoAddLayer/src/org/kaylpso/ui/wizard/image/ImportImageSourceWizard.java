/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kaylpso.ui.wizard.image;

import java.net.MalformedURLException;

import javax.xml.bind.JAXBException;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.outline.GisMapOutlineViewer;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.wizard.data.IKalypsoDataImportWizard;
import org.kaylpso.ui.action.AddThemeCommand;

/**
 * ImportImageSourceWizard
 * <p>
 * 
 * created by
 * 
 * @author kuepfer (21.05.2005)
 */
public class ImportImageSourceWizard extends Wizard implements IKalypsoDataImportWizard
{

  private GisMapOutlineViewer m_outlineviewer;

  private ImportImageWizardPage m_page;

  /**
   * @see org.eclipse.jface.wizard.IWizard#performFinish()
   */
  public boolean performFinish()
  {
    IMapModell mapModell = m_outlineviewer.getMapModell();
    if( m_outlineviewer.getMapModell() != null )
    {
      try
      {
        AddThemeCommand command = new AddThemeCommand( (GisTemplateMapModell)mapModell, m_page.getRelativeSourcePath()
            .removeFileExtension().lastSegment(), m_page.getFileType(), null, m_page.getURL().toString() + "#"
            + m_page.getCSName() );
        m_outlineviewer.postCommand( command, null );
      }
      catch( JAXBException e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
        return false;
      }
      // TODO Auto-generated method stub
      catch( MalformedURLException e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
        return false;
      }
    }
    m_page.dispose();
    return true;
  }

  /**
   * @see org.kalypso.ui.wizard.data.IKalypsoDataImportWizard#setOutlineViewer(org.kalypso.ogc.gml.outline.GisMapOutlineViewer)
   */
  public void setOutlineViewer( GisMapOutlineViewer outlineviewer )
  {
    m_outlineviewer = outlineviewer;

  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( IWorkbench workbench, IStructuredSelection selection )
  {
  // TODO Auto-generated method stub

  }

  public void addPages()
  {

    m_page = new ImportImageWizardPage( "imageimport", "Bild importieren (tif, jpg, png)",
        ImageProvider.IMAGE_KALYPSO_ICON_BIG );
    if( m_outlineviewer != null )
    {
      m_page.setProjectSelection( m_outlineviewer.getMapModell().getProject() );
    }
    addPage( m_page );

  }

  // @Christoph: wurde nicht benutzt und gibts auch schon in IProjekt.
  //  private String getRelativeProjectPath( IPath path )
  //  {
  //    return "project:/" + path.removeFirstSegments( 1 ).toString();
  //  }

}