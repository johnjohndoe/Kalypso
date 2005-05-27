package org.kaylpso.ui.wizard.wfs;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.ArrayList;

import javax.naming.OperationNotSupportedException;

import org.apache.commons.io.IOUtils;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.outline.GisMapOutlineViewer;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.wizard.data.IKalypsoDataImportWizard;
import org.kaylpso.ui.KalypsoServiceConstants;
import org.kaylpso.ui.action.AddThemeCommand;

/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/

/**
 * 
 * @author Kuepferle
 *  
 */
public class ImportWfsSourceWizard extends Wizard implements IKalypsoDataImportWizard
{
  private ImportWfsWizardPage m_page;

  private GisMapOutlineViewer m_outlineviewer;

  private ArrayList m_catalog;

  public ImportWfsSourceWizard()
  {
    super();

  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#performFinish()
   */
  public boolean performFinish()
  {
    IMapModell mapModell = m_outlineviewer.getMapModell();
    if( m_outlineviewer.getMapModell() != null )
      try
      {
        String[] layers = m_page.getSelectedFeatureNames();
        for( int i = 0; i < layers.length; i++ )
        {
          String layer = layers[i];
          //Write the defaultStyle to the system-default temporary directory
          URL style = m_page.setDefautltStyle( layer );

          // TODO here the featurePath is set to featureMember because this is
          // the top feature of the GMLWorkspace
          // it must be implemented to only set the name of the feature
          // (relative path of feature)
          String featurePath = m_page.guessFeaturePath( layer );
          if( featurePath == null )
            throw new OperationNotSupportedException(
                "The guessing of feature path has failed. The user has to choose the feature path, not implemented yet" );

          AddThemeCommand command = new AddThemeCommand( (GisTemplateMapModell)mapModell, layer,
              "wfs", featurePath, "URL=" + m_page.getUrl() + "#" + "FEATURE=" + layer, "sld",
              layer, style.toString(), "simple" );
          m_outlineviewer.postCommand( command, null );

        }

      }
      catch( Exception e )
      {
        e.printStackTrace();
        return false;
      }
    m_page.removeListeners();
    return true;
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( IWorkbench workbench, IStructuredSelection selection )
  {
    InputStream is = getClass().getResourceAsStream( "resources/kalypsoOWS.catalog" );
    try
    {
      // read service catalog file
      readCatalog( is );
      is.close();
    }
    catch( IOException e )
    {
      e.printStackTrace();
      m_catalog = new ArrayList();
    }
    finally
    {
      IOUtils.closeQuietly(is);
    }
  }

  public void addPages()
  {
    m_page = new ImportWfsWizardPage( "WfsImportPage", "Web Feature Service einbinden",
        ImageProvider.IMAGE_UTIL_UPLOAD_WIZ );
    addPage( m_page );

  }

  public boolean performCancel()
  {

    this.dispose();
    return true;
  }

  /**
   * 
   * @see org.kalypso.ui.wizard.data.IKalypsoDataImportWizard#setOutlineViewer(org.kalypso.ogc.gml.outline.GisMapOutlineViewer)
   */
  public void setOutlineViewer( GisMapOutlineViewer outlineviewer )
  {
    m_outlineviewer = outlineviewer;
  }

  public ArrayList getCatalog()
  {
    return m_catalog;
  }

  public void readCatalog( InputStream is ) throws IOException
  {
    ArrayList catalog = new ArrayList();
    BufferedReader br = new BufferedReader( new InputStreamReader( is ) );
    String line = br.readLine();
    do
    {
      if( line.startsWith( KalypsoServiceConstants.WFS_LINK_TYPE ) )
        catalog.add( ( line.split( "=" ) )[1] );

      line = br.readLine();
    }
    while( line != null );

    m_catalog = catalog;
  }
}