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
package org.kalypso.model.wspm.tuhh.ui.chart.layers;

import java.math.BigDecimal;

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.gml.classifications.IRoughnessClass;
import org.kalypso.model.wspm.core.gml.classifications.helper.WspmClassifications;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyRemove;
import org.kalypso.model.wspm.core.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.core.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.view.ILayerStyleProvider;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer;
import org.kalypso.observation.result.IComponent;

import de.openali.odysseus.chart.framework.model.data.IDataRange;
import de.openali.odysseus.chart.framework.model.data.impl.DataRange;
import de.openali.odysseus.chart.framework.model.figure.impl.FullRectangleFigure;
import de.openali.odysseus.chart.framework.model.mapper.IAxis;
import de.openali.odysseus.chart.framework.model.style.IPointStyle;
import de.openali.odysseus.chart.framework.model.style.impl.AreaStyle;
import de.openali.odysseus.chart.framework.model.style.impl.ColorFill;
import de.openali.odysseus.chart.framework.util.StyleUtils;

/**
 * @author kimwerner
 */
public class RoughnessLayer extends AbstractProfilLayer
{
  private IPointStyle m_styleClazzes;

  public RoughnessLayer( final IProfil profil, final String targetRangeProperty, final ILayerStyleProvider styleProvider )
  {
    super( IWspmTuhhConstants.LAYER_RAUHEIT, profil, targetRangeProperty, styleProvider );
  }

  @Override
  public Rectangle getHoverRect( final IProfileRecord profilPoint )
  {
    // TODO get HoverInfo
    return null;
  }

  @Override
  public void paint( final GC gc )
  {
    final IProfil profil = getProfil();
    if( profil == null )
      return;

    if( !hasRoughnessProperties() )
      return;

    final int baseLine = getTargetAxis().getScreenHeight();
    final FullRectangleFigure fr = new FullRectangleFigure();

    final IPointStyle ps = getPointStyle();
    final AreaStyle as = new AreaStyle( new ColorFill( ps.getInlineColor() ), ps.getAlpha(), ps.getStroke(), ps.isVisible() );
    fr.setStyle( as );
    final IAxis dom = getDomainAxis();
    final IAxis tar = getTargetAxis();

    final IProfileRecord[] points = profil.getPoints();

    for( final IProfileRecord point : points )
    {
      final IProfileRecord next = point.getNextPoint();
      if( Objects.isNull( next ) )
        continue;

      final Double px1 = point.getBreite();
      final Double py1 = getValue( point );

      final Double px2 = next.getBreite();
      if( Objects.isNull( px1, py1, px2 ) )
        continue;

      final int x1 = dom.numericToScreen( px1 );
      final int y1 = tar.numericToScreen( py1 );
      final int x2 = dom.numericToScreen( px2 );

      fr.setRectangle( new Rectangle( x1, y1, Math.abs( x2 - x1 ), Math.abs( baseLine - y1 ) ) );
      fr.paint( gc );
    }

  }

  private boolean hasRoughnessProperties( )
  {
    final IProfil profil = getProfil();
    final IComponent property = profil.getPointPropertyFor( getTargetProperty() );
    if( Objects.isNull( property ) )
    {
      if( Objects.isNull( profil.getProperty( IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_CLASS ) ) )
        return false;
    }

    return true;
  }

  @Override
  protected IPointStyle getPointStyle( )
  {
    if( IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_CLASS.equals( getTargetProperty() ) )
    {
      if( Objects.isNotNull( m_styleClazzes ) )
        return m_styleClazzes;

      m_styleClazzes = StyleUtils.getDefaultPointStyle();
      m_styleClazzes.getStroke().setColor( new RGB( 0, 0, 0 ) );
      m_styleClazzes.setInlineColor( new RGB( 137, 62, 16 ) );
      m_styleClazzes.setAlpha( 40 );

      return m_styleClazzes;

    }

    return super.getPointStyle();
  }

  private Double getValue( final IProfileRecord point )
  {
    /**
     * TODO: 3 RoughnessLayer are initiated by default: ks, kst and roughness class layers. if a profile defines both ks
     * and kst values so we must show booth roughness class values, too. at the moment only roughness class ks values
     * will be shown!
     */
    /**
     * TODO 2: like calculation core, displaying / handling of roughness is configruated by a flag (use roughness
     * classes, use plain values)
     */
    final Double factor = point.getRoughnessFactor();
    final IComponent component = getTargetComponent();
    if( Objects.isNotNull( component ) )
    {
      final Object value = point.getValue( component );
      if( value instanceof Number )
      {
        final Number number = (Number) value;
        return number.doubleValue() * factor;
      }
    }

    final IRoughnessClass clazz = WspmClassifications.findRoughnessClass( point );
    if( Objects.isNull( clazz ) )
      return null;

    final String target = getTargetProperty();
    switch( target )
    {
      case IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS:
        final BigDecimal clazzKsValue = clazz.getKsValue();
        if( Objects.isNotNull( clazzKsValue ) )
          return clazzKsValue.doubleValue() * factor;

      case IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST:
        final BigDecimal clazzKstValue = clazz.getKstValue();
        if( Objects.isNotNull( clazzKstValue ) )
          return clazzKstValue.doubleValue() * factor;

      default:
        return null;
    }
  }

  @Override
  public IDataRange< ? > getTargetRange( final IDataRange< ? > domainIntervall )
  {
    final IProfil profil = getProfil();
    if( Objects.isNull( profil ) )
      return null;

    Double min = Double.MAX_VALUE;
    Double max = -Double.MAX_VALUE;
    final IProfileRecord[] points = profil.getPoints();
    for( final IProfileRecord point : points )
    {
      final Double value = getValue( point );
      if( Objects.isNotNull( value ) )
      {
        min = Math.min( min, value );
        max = Math.max( max, value );
      }
    }

    return new DataRange<Double>( min, max );
  }

  @Override
  public void removeYourself( )
  {
    final IProfil profil = getProfil();

    final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.RoughnessLayer.0" ), getProfil(), true ); //$NON-NLS-1$
    operation.addChange( new PointPropertyRemove( profil, getTargetComponent() ) );
    new ProfilOperationJob( operation ).schedule();

  }
}
