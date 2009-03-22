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
package org.kalypso.model.wspm.tuhh.ui.chart;

import org.eclipse.swt.graphics.RGB;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.TuhhProfil;
import org.kalypso.model.wspm.ui.view.AbstractLayerStyleProvider;

import de.openali.odysseus.chart.framework.model.style.ILineStyle;
import de.openali.odysseus.chart.framework.model.style.IPointStyle;
import de.openali.odysseus.chart.framework.model.style.impl.LineStyle;
import de.openali.odysseus.chart.framework.model.style.impl.PointStyle;

/**
 * @author kimwerner
 */
public class LayerStyleProviderTuhh extends AbstractLayerStyleProvider
{
  @Override
  protected void createStyles( )
  {
    // TODO: read styles from *.kod file
    createPointMarkerSytles();
    createCrossSectionSytles();
    createRoughnessSytles();
    createBridgeSytles();
    createWeirSytles();
    createVegetationStyle();
  }

  private void createPointMarkerSytles( )
  {
    final IProfilPointMarkerProvider markerProvider = KalypsoModelWspmCoreExtensions.getMarkerProviders( TuhhProfil.PROFIL_TYPE );

    final ILineStyle lsT = getStyleFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE + "_LINE", LineStyle.class ); //$NON-NLS-1$
    lsT.setColor( markerProvider.getColorFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
    lsT.setWidth( 2 );

    final ILineStyle lsD = lsT.copy();
    lsD.setColor( markerProvider.getColorFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) );
    addStyle( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE + "_LINE", lsD ); //$NON-NLS-1$

    final ILineStyle lsB = lsT.copy();
    lsB.setColor( markerProvider.getColorFor( IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) );
    addStyle( IWspmTuhhConstants.MARKER_TYP_BORDVOLL + "_LINE", lsB ); //$NON-NLS-1$
  }

  private void createWeirSytles( )
  {
    final IProfilPointMarkerProvider markerProvider = KalypsoModelWspmCoreExtensions.getMarkerProviders( TuhhProfil.PROFIL_TYPE );
    final RGB col = markerProvider.getColorFor( IWspmTuhhConstants.MARKER_TYP_WEHR );

    final ILineStyle ls = getStyleFor( IWspmTuhhConstants.MARKER_TYP_WEHR + "_LINE", LineStyle.class ); //$NON-NLS-1$
    ls.setColor( col );
    addStyle( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR + "_LINE", ls.copy() ); //$NON-NLS-1$
  }

  private void createCrossSectionSytles( )
  {
    final ILineStyle ls = getStyleFor( IWspmTuhhConstants.POINT_PROPERTY_HOEHE + "_LINE", LineStyle.class ); //$NON-NLS-1$
    ls.setColor( new RGB( 255, 150, 0 ) );
  }

  private void createVegetationStyle( )
  {
    getStyleFor( IWspmTuhhConstants.LAYER_BEWUCHS + "_LINE", LineStyle.class ).setColor( new RGB( 0, 255, 0 ) ); //$NON-NLS-1$
  }

  private void createBridgeSytles( )
  {
    final ILineStyle lsO = getStyleFor( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE + "_LINE", LineStyle.class ); //$NON-NLS-1$
    lsO.setColor( new RGB( 0, 128, 0 ) );

    final ILineStyle lsU = lsO.copy();
    lsU.setColor( new RGB( 0, 128, 179 ) );
    addStyle( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE + "_LINE", lsU ); //$NON-NLS-1$
  }

  private void createRoughnessSytles( )
  {
    final IPointStyle psKS = getStyleFor( IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KS + "_POINT", PointStyle.class ); //$NON-NLS-1$
    psKS.getStroke().setColor( new RGB( 0, 0, 0 ) );
    psKS.setInlineColor( new RGB( 0, 0, 0 ) );
    psKS.setAlpha( 50 );
    addStyle( IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KST + "_POINT", psKS.copy() ); //$NON-NLS-1$
  }

}
