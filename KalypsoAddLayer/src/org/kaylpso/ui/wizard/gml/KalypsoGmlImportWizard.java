package org.kaylpso.ui.wizard.gml;

import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.deegree.services.wms.StyleNotDefinedException;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.outline.GisMapOutlineViewer;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.gmleditor.util.model.FeatureElement;
import org.kalypso.ui.editor.gmleditor.util.model.IModel;
import org.kalypso.ui.editor.gmleditor.util.model.PropertyElement;
import org.kalypso.ui.wizard.data.IKalypsoDataImportWizard;
import org.kalypsodeegree.model.feature.FeatureType;
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
 * */
public class KalypsoGmlImportWizard extends Wizard implements IKalypsoDataImportWizard
{
  private GisMapOutlineViewer m_outlineviewer;

  private GmlFileImportPage m_page;

  public KalypsoGmlImportWizard()
  {
    super();
  }

  public void addPages()
  {

    m_page = new GmlFileImportPage( "GML:importPage",
        "Hinzufügen einer GML-Datei (im Workspace) zu einer Karte",
        ImageProvider.IMAGE_UTIL_UPLOAD_WIZ );
      m_page.setProjectSelection( m_outlineviewer.getMapModell().getProject() );
    if( m_outlineviewer != null )
      m_page.setMapContextURL( ( (GisTemplateMapModell)m_outlineviewer.getMapModell() )
          .getContext() );
    addPage( m_page );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  public boolean performFinish()
  {
    System.out.print( false );
    IModel selection = m_page.getSelection();
    IMapModell mapModell = m_outlineviewer.getMapModell();
    String featurePath = buildFeaturePath( selection );
    String featureName = null;
    URL styleHref = null;
    try
    {
      if( selection instanceof FeatureElement )
      {
        featureName = m_page.getFeature().getFeatureType().getName();
        featurePath += "[" + featureName + "]";
        styleHref = KalypsoGisPlugin.getDefault().getDefaultStyleFactory().getDefaultStyle(
            m_page.getFeature().getFeatureType() );
      }
      else if( selection instanceof PropertyElement )
      {
        featureName = m_page.getFatp().getName();
        styleHref = KalypsoGisPlugin.getDefault().getDefaultStyleFactory().getDefaultStyle(
            (FeatureType)m_page.getFatp() );
      }

      AddThemeCommand command = new AddThemeCommand( (GisTemplateMapModell)mapModell, featureName,
          "gml", featurePath, m_page.getSource(), "sld", featureName, styleHref.toString(),
          "simple" );
      m_outlineviewer.postCommand( command, null );
    }
    catch( StyleNotDefinedException e )
    {
      // TODO: handle exception
    }
    catch( Exception e )
    {
      // TODO: handle exception
    }
    m_page.removerListeners();
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
   // nothing to initalize
  }

  private boolean getParent( IModel element, List list )
  {
    if( element != null )
    {
      if( element instanceof FeatureElement )
      {
        getParent( element.getParent(), list );
      }
      else if( element instanceof PropertyElement )
      {
        list.add( element.getName() );
        getParent( element.getParent(), list );
      }
      return true;
    }
    return false;
  }

  public String buildFeaturePath( IModel selection )
  {

    List list = new ArrayList();
    while( !getParent( selection, list ) )
      break;

    String featurePath = null;
    for( int i = list.size(); i > 0; i-- )
    {
      String name = (String)list.get( i - 1 );
      if( i < list.size() )
        featurePath += "/" + name;
      else
        featurePath = name;
    }
    return featurePath;
  }

}