/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.ui.wizard.wms;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Properties;
import java.util.Set;
import java.util.Map.Entry;

import org.apache.commons.io.IOUtils;
import org.deegree.services.wms.capabilities.Layer;
import org.deegree.services.wms.capabilities.Style;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.map.themes.KalypsoWMSTheme;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.outline.GisMapOutlineViewer;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoServiceConstants;
import org.kalypso.ui.action.AddThemeCommand;
import org.kalypso.ui.wizard.IKalypsoDataImportWizard;

/**
 * @author Kuepferle
 */
public class ImportWmsSourceWizard extends Wizard implements IKalypsoDataImportWizard
{
  private ImportWmsWizardPage m_page = null;

  private GisMapOutlineViewer m_outlineviewer;

  private final ArrayList<String> m_catalog = new ArrayList<String>();

  /**
   * @see org.eclipse.jface.wizard.IWizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    IMapModell mapModell = m_outlineviewer.getMapModell();
    if( m_outlineviewer.getMapModell() != null )

      try
      {
        final boolean isMulti = m_page.isMultiLayer();
        final StringBuffer source = new StringBuffer( KalypsoWMSTheme.KEY_URL + "=" + m_page.getBaseURL().toString() );
        final StringBuffer layers = new StringBuffer( KalypsoWMSTheme.KEY_LAYERS + "=" );
        final StringBuffer styles = new StringBuffer( KalypsoWMSTheme.KEY_STYLES + "=" );
        final Layer[] layerArray = m_page.getLayersList();

        if( isMulti )
        {
          for( int i = 0; i < layerArray.length; i++ )
          {
            final Layer layer = layerArray[i];
            final String layerName = layer.getName();
            final String styleName;
            final Style[] styles2 = layer.getStyles();
            if( styles2.length > 0 )
              styleName = styles2[0].getName();
            else
              styleName = "default";
            layers.append( layerName );
            styles.append( styleName );
            if( i < layerArray.length - 1 )
            {
              layers.append( "," );
              styles.append( "," );
            }
          }
          final String layerName = "Multi" + source;
          source.append( "#" ).append( layers.toString() );
          source.append( "#" ).append( styles.toString() );
          final AddThemeCommand command = new AddThemeCommand( (GisTemplateMapModell) mapModell, layerName, "wms", null, source.toString() );
          m_outlineviewer.postCommand( command, null );
        }
        else
        {
          for( int i = 0; i < layerArray.length; i++ )
          {
            final Layer layer = layerArray[i];
            final String layerName = layer.getName();
            final String styleName;
            final Style[] styles2 = layer.getStyles();
            if( styles2.length > 0 )
              styleName = styles2[0].getName();
            else
              styleName = "default";

            final String layerTitle = layer.getTitle();
            source.append( "#" ).append( KalypsoWMSTheme.KEY_LAYERS ).append( "=" ).append( layerName );
            source.append( "#" ).append( KalypsoWMSTheme.KEY_STYLES ).append( "=" ).append( styleName );
            final AddThemeCommand command = new AddThemeCommand( (GisTemplateMapModell) mapModell, layerTitle, "wms", null, source.toString() );
            m_outlineviewer.postCommand( command, null );
          }

        }
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    return true;
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( final IWorkbench workbench, IStructuredSelection selection )
  {
    // read service catalog file
    InputStream is = getClass().getResourceAsStream( "resources/kalypsoOWS.catalog" );
    try
    {
      readCatalog( is );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      
      m_catalog.clear();
    }
    finally
    {
      IOUtils.closeQuietly( is );
    }
  }

  @Override
  public void addPages( )
  {
    m_page = new ImportWmsWizardPage( "WmsImportPage", "Web Map Service einbinden", ImageProvider.IMAGE_UTIL_UPLOAD_WIZ );
    addPage( m_page );
  }

  /**
   * @see org.kalypso.ui.wizard.data.IKalypsoDataImportWizard#setOutlineViewer(org.kalypso.ogc.gml.outline.GisMapOutlineViewer)
   */
  public void setOutlineViewer( GisMapOutlineViewer outlineviewer )
  {
    m_outlineviewer = outlineviewer;
  }

  public ArrayList<String> getCatalog( )
  {
    return m_catalog;
  }

  public void readCatalog( final InputStream is ) throws IOException
  {
    m_catalog.clear();
    
    // use properties to parse catalog: dont do everything yourself
    // fixes bug with '=' inside of URLs
    final Properties properties = new Properties();
    properties.load( is );
    
    final Set<Entry<Object, Object>> name = properties.entrySet();
    for( final Entry<Object, Object> entry : name )
    {
      if( entry.getKey().toString().startsWith( KalypsoServiceConstants.WMS_LINK_TYPE ) )
        m_catalog.add( entry.getValue().toString() );
    }
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#needsProgressMonitor()
   */
  @Override
  public boolean needsProgressMonitor( )
  {
    return true;
  }
}