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
package org.kalypso.ogc.gml.map.widgets.newfeature;

import java.awt.Point;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.ISourceProviderListener;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.MapPanelSourceProvider;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypso.ui.editor.mapeditor.views.MapWidgetView;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;

/**
 * Creates a new feature in the current theme and lets the user draw the associated geometries. The feature type of the
 * feature to be created can be specified by the user if more than one feature type is known that fits the theme. A
 * feature created this way is ensured to have all required geometry properties set.
 * 
 * @author kurzbach
 */
public class NewFeatureWidget extends AbstractCreateGeometryWidget implements IWidgetWithOptions
{

  private ISourceProviderListener m_sourceProviderListener;

  public NewFeatureWidget( final QName name, final QName[] geomProperties )
  {
    super( name, geomProperties );

    m_sourceProviderListener = new ISourceProviderListener()
    {

      public void sourceChanged( int sourcePriority, Map sourceValuesByName )
      {
        cancel();
      }

      public void sourceChanged( int sourcePriority, String sourceName, Object sourceValue )
      {
        cancel();
      }

      private void cancel( )
      {
        final IWorkbench workbench = PlatformUI.getWorkbench();
        if( workbench == null || workbench.isClosing() )
          return;

        final IWorkbenchWindow activeWorkbenchWindow = workbench.getActiveWorkbenchWindow();
        if( activeWorkbenchWindow == null )
          return;

        final IWorkbenchPage activePage = activeWorkbenchWindow.getActivePage();
        if( activePage == null )
          return;

        final Display display = workbench.getDisplay();
        display.asyncExec( new Runnable()
        {
          public void run( )
          {
            final MapPanel mapPanel = getMapPanel();
            if( mapPanel != null )
              mapPanel.getWidgetManager().setActualWidget( null );

            final IViewPart widgetView = activePage.findView( MapWidgetView.ID );
            if( widgetView != null )
              activePage.hideView( widgetView );
          }
        } );
      }

    };
    MapPanelSourceProvider.getInstance().addSourceProviderListener( m_sourceProviderListener );
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#createControl(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.ui.forms.widgets.FormToolkit)
   */
  public Control createControl( Composite parent, FormToolkit toolkit )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#disposeControl()
   */
  public void disposeControl( )
  {
    // TODO Auto-generated method stub

  }

}
