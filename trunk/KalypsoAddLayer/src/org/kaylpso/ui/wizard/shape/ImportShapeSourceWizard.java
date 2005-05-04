package org.kaylpso.ui.wizard.shape;

import java.net.MalformedURLException;
import java.util.List;

import javax.xml.bind.JAXBException;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.outline.GisMapOutlineViewer;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.wizard.data.IKalypsoDataImportWizard;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_LineString;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Surface;
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
 * */
public class ImportShapeSourceWizard extends Wizard implements IKalypsoDataImportWizard
{
  private ImportShapeFileImportPage m_page;

  private GisMapOutlineViewer m_outlineviewer;

  private IProject[] m_selectedProject;

  private String m_stylePath = null;
  
  public ImportShapeSourceWizard()
  {
    super();

  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#performFinish()
   */
  public boolean performFinish()
  {

    try
    {
      IPath targetPath = m_page.getProjectWorkspaceLocation();
      //copy Shape file to workspace
      String shapeBase = m_page.getShapeBase();
      //read Shapefile from local file system into a GMLWorkspace
      GMLWorkspace gml = ShapeSerializer.deserialize( shapeBase, m_page.getCRS(), null );

      String defaultStyle = null;
      defaultStyle = guessGeometryType( gml.getRootFeature() );
      if( defaultStyle == null )
        throw new UnsupportedOperationException( "no style available for geometry!" );

      IPath root = ResourcesPlugin.getWorkspace().getRoot().getLocation();
      int index = shapeBase.lastIndexOf( "/" );
      String target = root.append( targetPath + shapeBase.substring( index ) ).toFile().toString();
      ShapeSerializer.serialize( gml, target );

      //TODO When there is a styles registry get the path from the preferences
      m_stylePath = "project:/.styles/";
      String styleHref = m_stylePath + defaultStyle;
      //Add Layer to mapModell
      try
      {
        ResourcesPlugin.getWorkspace().getRoot().refreshLocal( IResource.DEPTH_INFINITE, null );
        IMapModell mapModell = m_outlineviewer.getMapModell();
        String filename = shapeBase.substring( index + 1 );
        String relative = m_page.getRelativeProjectContext() + filename + "#"
            + m_page.getCRS().getName();

        if( m_outlineviewer.getMapModell() != null && !m_page.isNewMapRequested() )
        {

          index = shapeBase.lastIndexOf( "/" );
          //        TODO here the featurePath is set to featureMember because this is
          // the root element of the GMLWorkspace
          //        it must be implemented to only set the name of the feature
          // (relative path of feature)
          AddThemeCommand command = new AddThemeCommand( (GisTemplateMapModell)mapModell, filename,
              "shape", "featureMember", relative, "sld", "default", styleHref, "simple" );
          m_outlineviewer.postCommand( command, null );
        }
        //if the user has choosen a new Map to be generated
        //TODO is not working yet
        else if( m_page.isNewMapRequested() )
        {
          Gismapview gismapview = createEmtpyMap();
          GisTemplateMapModell modell = new GisTemplateMapModell( gismapview, m_page
              .getResourceContextURL(), KalypsoGisPlugin.getDefault().getCoordinatesSystem(),
              m_selectedProject[0] );
          AddThemeCommand command = new AddThemeCommand( modell, filename, "shape",
              "featureMember", relative, "sld", "default", styleHref, "simple" );
          m_outlineviewer.postCommand( command, null );
        }
        //TODO create default map in project root
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

    catch( GmlSerializeException e )
    {
      e.printStackTrace();
      // TODO
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
    //do nothing
  }

  public void addPages()
  {

    m_page = new ImportShapeFileImportPage( "shapefileimport", "ESRI(tm) ein Projekt importieren",
        ImageProvider.IMAGE_KALYPSO_ICON_BIG );
    m_page.setProjectSelection( m_outlineviewer.getMapModell().getProject() );
    if( m_outlineviewer != null )
      m_page.setMapContextURL( ( (GisTemplateMapModell)m_outlineviewer.getMapModell() )
          .getContext() );
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

  private String guessGeometryType( Feature root )
  {

    final List features = (List)root.getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );
    Feature fistFeature = (Feature)features.get( 0 );
    GM_Object[] geom = fistFeature.getGeometryProperties();
    for( int i = 0; i < geom.length; i++ )
    {
      GM_Object property = geom[i];
      {
        if( property instanceof GM_Point )
          return "pointdefault.sld";
        if( property instanceof GM_LineString )
          return "linestringdefault.sld";
        if( property instanceof GM_Surface )
          return "polygondefault.sld";

      }
    }
    return null;
  }

  public Gismapview createEmtpyMap()
  {
    GM_Envelope bbox = m_outlineviewer.getMapModell().getFullExtentBoundingBox();
    Gismapview gismapview = null;
    try
    {
      gismapview = GisTemplateHelper.emptyGisView( bbox );
    }
    catch( JAXBException e )
    {
      e.printStackTrace();
      // TODO
    }
    return gismapview;
  }
}