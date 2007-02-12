package org.kalypso.ui.wizard.roughness;

import java.net.MalformedURLException;
import java.net.URL;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.outline.GisMapOutlineViewer;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.action.AddThemeCommand;
import org.kalypso.ui.wizard.IKalypsoDataImportWizard;
import org.kalypsodeegree.model.feature.GMLWorkspace;

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

public class ImportRoughnessSourceWizard extends Wizard implements IKalypsoDataImportWizard
{

  private GisMapOutlineViewer m_outlineviewer;

  private ImportRoughessSourceWizardPage m_page;

  private IProject m_project;

  private URL m_mapContextURL;

  public ImportRoughnessSourceWizard( )
  {
    super();
  }

  @Override
  public void addPages( )
  {
    m_page = new ImportRoughessSourceWizardPage( "Add RoughnessDataModel", "Add roughness", ImageProvider.IMAGE_KALYPSO_ICON_BIG );
    if( m_project != null )
      m_page.setProject( m_project );
    addPage( m_page );

  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {

    IPath filePath = m_page.getFilePath();

    String stylePath = null;

    String styleName = null;

    if( m_page.checkDefaultStyle() )
    {
      UrlResolver urlResolver = new UrlResolver();
      final URL gmlURL;
      try
      {
        gmlURL = urlResolver.resolveURL( m_mapContextURL, getRelativeProjectPath( filePath ) );
        GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( gmlURL, urlResolver, null );
        
        IFeatureType ft = workspace.getFeatureType( new QName(NS.GML3, "RoughnessPolygon", null ));
        styleName = ft.getName();
        stylePath = KalypsoGisPlugin.getDefaultStyleFactory().getDefaultStyle( ft, styleName ).toString();
      }
      catch( MalformedURLException e1 )
      {
        e1.printStackTrace();
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }
    else
    {
      stylePath = getRelativeProjectPath( m_page.getStylePath() );
      styleName = m_page.getStyleName();
    }

    // Add Layer to mapModell
    IMapModell mapModell = m_outlineviewer.getMapModell();
    if( m_outlineviewer.getMapModell() != null )
      try
      {
        AddThemeCommand command = new AddThemeCommand( (GisTemplateMapModell) mapModell, "Roughness", "gml", "#fid#RoughnessLayerPolygonCollection11709431308431/roughnessLayerMember[RoughnessPolygon]", getRelativeProjectPath( filePath ), "sld", styleName, stylePath, "simple" );
        m_outlineviewer.postCommand( command, null );
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }

    return true;
  }

  private String getRelativeProjectPath( IPath path )
  {
    return "project:/" + path.removeFirstSegments( 1 ).toString();
  }

  /**
   * @see org.kalypso.ui.wizard.data.IKalypsoDataImportWizard#setOutlineViewer(org.kalypso.ogc.gml.outline.GisMapOutlineViewer)
   */
  public void setOutlineViewer( GisMapOutlineViewer outlineviewer )
  {
    m_outlineviewer = outlineviewer;
    m_project = m_outlineviewer.getMapModell().getProject();
    m_mapContextURL = ((GisTemplateMapModell) m_outlineviewer.getMapModell()).getContext();
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( IWorkbench workbench, IStructuredSelection selection )
  {
    // nothing
  }
}