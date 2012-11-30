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
package org.kalypso.model.wspm.tuhh.core.profile.sinuositaet;

import org.eclipse.core.runtime.Assert;
import org.kalypso.model.wspm.core.profil.impl.AbstractProfileObject;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;

/**
 * @author Dirk Kuch
 * @author Holger Albert
 */
public class SinuositaetProfileObject extends AbstractProfileObject implements ISinuositaetProfileObject
{
  public static final String ID = "urn:ogc:gml:dict:kalypso:model:wspm:tuhh:core:sinuositaetTypes#SINUOSITAET"; //$NON-NLS-1$

  private static final SINUOSITAET_KENNUNG DEFAULT_KENNUNG = SINUOSITAET_KENNUNG.eBeides;

  private static final SINUOSITAET_GERINNE_ART DEFAULT_GERINNE_ART = SINUOSITAET_GERINNE_ART.eGegliedert;

  public static final String PROPERTY_KENNUNG = "kennung"; //$NON-NLS-1$

  public static final String PROPERTY_SN = "sn"; //$NON-NLS-1$

  public static final String PROPERTY_GERINNE_ART = "gerinneArt"; //$NON-NLS-1$

  public static final String PROPERTY_LF = "lf"; //$NON-NLS-1$

  public SinuositaetProfileObject( )
  {
    super();

    setKennung( DEFAULT_KENNUNG );
    setGerinneArt( DEFAULT_GERINNE_ART );
  }

  @Override
  public String getType( )
  {
    return ID;
  }

  @Override
  public String[] getProperties( )
  {
    return new String[] { PROPERTY_KENNUNG, PROPERTY_SN, PROPERTY_GERINNE_ART, PROPERTY_LF };
  }

  @Override
  public String getPropertyLabel( final String property )
  {
    if( PROPERTY_KENNUNG.equals( property ) )
      return Messages.getString("SinuositaetProfileObject_0"); //$NON-NLS-1$

    if( PROPERTY_SN.equals( property ) )
      return Messages.getString("SinuositaetProfileObject_1"); //$NON-NLS-1$

    if( PROPERTY_GERINNE_ART.equals( property ) )
      return Messages.getString("SinuositaetProfileObject_2"); //$NON-NLS-1$

    if( PROPERTY_LF.equals( property ) )
      return Messages.getString("SinuositaetProfileObject_3"); //$NON-NLS-1$

    return property;
  }

  @Override
  public SINUOSITAET_KENNUNG getKennung( )
  {
    final String value = getValue( KEY_KENNUNG, null );
    if( value == null )
      return DEFAULT_KENNUNG;

    return SINUOSITAET_KENNUNG.valueOf( value );
  }

  @Override
  public Double getSn( )
  {
    return getDoubleValue( KEY_SN, null );
  }

  @Override
  public SINUOSITAET_GERINNE_ART getGerinneArt( )
  {
    final String value = getValue( KEY_GERINNE_ART, null );
    if( value == null )
      return DEFAULT_GERINNE_ART;

    return SINUOSITAET_GERINNE_ART.valueOf( value );
  }

  @Override
  public Double getLf( )
  {
    return getDoubleValue( KEY_LF, null );
  }

  public void setKennung( final String kennung )
  {
    setValue( KEY_KENNUNG, kennung );
  }

  public void setKennung( final SINUOSITAET_KENNUNG kennung )
  {
    Assert.isNotNull( kennung );
    setValue( KEY_KENNUNG, kennung.name() );
  }

  public void setSn( final Double sn )
  {
    setDoubleValue( KEY_SN, sn );
  }

  public void setGerinneArt( final String gerinneArt )
  {
    setValue( KEY_GERINNE_ART, gerinneArt );
  }

  public void setGerinneArt( final SINUOSITAET_GERINNE_ART gerinneArt )
  {
    Assert.isNotNull( gerinneArt );
    setValue( KEY_GERINNE_ART, gerinneArt.name() );
  }

  public void setLf( final Double lf )
  {
    setDoubleValue( KEY_LF, lf );
  }
}