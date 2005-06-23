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
package org.kalypso.ogc.gml.map.widgets;

import java.awt.Point;

import org.eclipse.swt.widgets.Shell;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.featureview.FeatureComposite;
import org.kalypso.ogc.gml.featureview.FeatureviewDialog;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.visitors.FindNearestVisitor;

/**
 * @author Belger
 */
public class EditFeatureWidget extends AbstractWidget
{
  private Point m_point;

  private Shell m_shell;

  public EditFeatureWidget( String name, String tooltip )
  {
    super( name, tooltip );
  }

  public void leftPressed( final Point p )
  {
    m_point = p;
    perform();
  }

  /**
   * 
   * @see org.kalypso.ogc.gml.widgets.IWidget#perform()
   */
  public void perform()
  {
    if( m_point != null )
    {
      final GM_Position position = getPosition( m_point );
      final IKalypsoTheme activeTheme = getActiveTheme();
      if( activeTheme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme featureTheme = (IKalypsoFeatureTheme)activeTheme;

        // TODO: Sollte so etwas nicht eine Zentrale Stelle machen?
        final GeoTransform transform = getMapPanel().getProjection();
        final double gisRadius = transform.getSourceX( m_point.getX() + 20 ) - position.getX();

        final FindNearestVisitor visitor = new FindNearestVisitor( position, gisRadius );
        final FeatureList featureList = featureTheme.getFeatureList();
        featureList.accept( visitor );
        final Feature result = visitor.getResult();
        if( result != null )
          editFeature( featureTheme, featureTheme.getWorkspace(), result );
      }
    }
  }

  private void editFeature( final ICommandTarget commandTarget, final GMLWorkspace workspace, final Feature feature )
  {
    if( m_shell != null && feature != null )
    {
      final FeatureComposite helper = new FeatureComposite( workspace, feature );

      final Shell shell = m_shell;
      shell.getDisplay().asyncExec( new Runnable()
      {
        public void run()
        {
          final FeatureviewDialog dialog = new FeatureviewDialog( workspace, commandTarget, shell, helper );
          dialog.open();
        }
      } );
    }
  }

  public void setShell( Shell shell )
  {
    m_shell = shell;
  }
}