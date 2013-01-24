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
package org.kalypso.kalypsomodel1d2d.ui.map.merge;

import java.awt.Graphics;
import java.awt.Image;
import java.awt.image.ImageObserver;

import javax.swing.ImageIcon;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.i18n.I10nString;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.ops.CalcUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.ogc.gml.AbstractKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * Theme that shows a calculation unit
 *
 * @author Patrice Congo
 *
 */
public class Model1d2dCalUnitTheme extends AbstractKalypsoTheme
{
  private CalUnitDisplayElement m_calUnitDisplayElement;

  private ICalculationUnit m_calcUnit;

  private IFlowRelationshipModel m_modelBoundaryConditions;

  private final ImageIcon m_imgIcon = new ImageIcon( PluginUtilities.findResource( KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(), "icons/elcl16/apply.png" ) ); //$NON-NLS-1$

  private final ModellEventListener m_modellListener = new ModellEventListener()
  {
    @SuppressWarnings("synthetic-access")
    @Override
    public void onModellChange( final ModellEvent modellEvent )
    {
      // We request repaint on any modell event, because we can assume, that we only get modell events
      // from the calc-unit workspace and this can only happen when editing the cal-unit
      fireRepaintRequested( null );
    }
  };

  public Model1d2dCalUnitTheme( final I10nString name, final IMapModell mapModel )
  {
    super( name, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.merge.Model1d2dCalUnitTheme.0" ), mapModel ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#dispose()
   */
  @Override
  public void dispose( )
  {
    if( m_calcUnit != null )
      m_calcUnit.getFeature().getWorkspace().removeModellListener( m_modellListener );

    super.dispose();
  }

  public void setCalculationUnit( final ICalculationUnit calcUnit )
  {
    if( m_calcUnit != null )
      m_calcUnit.getFeature().getWorkspace().removeModellListener( m_modellListener );

    m_calUnitDisplayElement = null;
    m_calcUnit = calcUnit;

    if( calcUnit != null )
    {
      m_calUnitDisplayElement = new CalUnitDisplayElement( calcUnit );
      calcUnit.getFeature().getWorkspace().addModellListener( m_modellListener );
    }

    fireRepaintRequested( null );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getBoundingBox()
   */
  @Override
  public GM_Envelope getFullExtent( )
  {
    if( m_calcUnit == null )
      return null;
    return CalcUnitOps.getBoundingBox( m_calcUnit );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, java.lang.Boolean,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus paint( final Graphics g, final GeoTransform p, final Boolean selected, final IProgressMonitor monitor )
  {
    if( selected != null && selected )
      return Status.OK_STATUS;

    if( m_calUnitDisplayElement != null )
      m_calUnitDisplayElement.paint( g, p, monitor );

    markAppliedBoundaryConditions( g, p );
    return Status.OK_STATUS;
  }

  private void markAppliedBoundaryConditions( final Graphics g, final GeoTransform p )
  {
    if( m_modelBoundaryConditions != null && m_calcUnit != null )
    {
      final Image img = m_imgIcon.getImage();
      final int yTrans = (int) (m_imgIcon.getIconHeight() / -2.0);
      final int xTrans = (int) (m_imgIcon.getIconWidth() / -2.0);
      for( final IFeatureWrapper2 bc : m_modelBoundaryConditions )
      {
        if( bc instanceof IBoundaryCondition )
        {
          if( CalcUnitOps.isBoundaryConditionOf( m_calcUnit, (IBoundaryCondition) bc ) )
          {
            final GM_Point position = ((IBoundaryCondition) bc).getPosition();
            final double gPosX = p.getDestX( position.getX() ) + xTrans;
            final double gPosY = p.getDestY( position.getY() ) + yTrans;
            final ImageObserver observer = null;
            g.drawImage( img, (int) gPosX, (int) gPosY, observer );
          }
        }
      }
    }
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#isLoaded()
   */
  @Override
  public boolean isLoaded( )
  {
    return true;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getType()
   */
  @Override
  public String getType( )
  {
    return "GML_MERGE"; //$NON-NLS-1$
  }

  public void setModelBoundaryConditions( final IFlowRelationshipModel bcs )
  {
    m_modelBoundaryConditions = bcs;
  }
}
