package org.kaylpso.ui.wizard.wms;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;

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
 *  Denickestraße 22
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
public class ImportWmsSourceWizard extends Wizard implements IKalypsoDataImportWizard
{
  private ImportWmsWizardPage m_page = null;

  private String[] m_layers;

  private GisMapOutlineViewer m_outlineviewer;

  private ArrayList m_catalog;

  private static final String LINK_TYPE = "wms";

  public ImportWmsSourceWizard()
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
        String url = "URL=" + m_page.getUrl().toString();
        //TODO verschlüsslung, samit in der gmt datei die info nicht gelesen
        // werden kann
        String authentification = "USER=" + m_page.getUserName() + "PASS=" + m_page.getPassWord();
        m_layers = m_page.getLayers();
        for( int i = 0; i < m_layers.length; i++ )
        {
          String layers = "LAYERS=";
          String layer = m_layers[i];
          String layername = layer;
          //TODO create a list and add to tooltip for layer ??
          if( m_layers.length == 1 && m_page.isMultiLayer() )
            layername = "Multi Layer:" + layername;
          layers = layers + layer;
          //          System.out.println( url + "#" + layers );
          AddThemeCommand command = new AddThemeCommand( (GisTemplateMapModell)mapModell,
              layername, "wms", null, url + "#" + layers + "#" + authentification, null, null,
              null, "simple" );
          m_outlineviewer.postCommand( command, null );

        }
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }

    m_page.removeListners();
    return true;
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( IWorkbench workbench, IStructuredSelection selection )
  {
    // read service catalog file
    InputStream is = getClass().getResourceAsStream( "resources/kalypsoOWS.catalog" );
    try
    {
      readCatalog( is );
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
    m_page = new ImportWmsWizardPage( "WmsImportPage", "Web Map Service einbinden",
        ImageProvider.IMAGE_UTIL_UPLOAD_WIZ );
    addPage( m_page );
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
      if( line.startsWith( KalypsoServiceConstants.WMS_LINK_TYPE ) )
        catalog.add( ( line.split( "=" ) )[1] );

      line = br.readLine();
    }
    while( line != null );
    
    m_catalog = catalog;
  }
}