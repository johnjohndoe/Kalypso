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

import org.kalypso.model.wspm.core.profil.AbstractProfileObject;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

/**
 * @author Dirk Kuch
 */
public class SinuositaetProfileObject extends AbstractProfileObject implements ISinuositaetProfileObject
{
  public SinuositaetProfileObject( )
  {
    this( buildObservation() );
  }

  private static IObservation<TupleResult> buildObservation( )
  {
    final TupleResult result = new TupleResult();
    result.addComponent( getObjectComponent( PROPERTY_KENNUNG ) );
    result.addComponent( getObjectComponent( PROPERTY_SN ) );
    result.addComponent( getObjectComponent( PROPERTY_GERINNE_ART ) );
    result.addComponent( getObjectComponent( PROPERTY_LF ) );

    final Observation<TupleResult> observation = new Observation<TupleResult>( ID, "Sinuosit‰t", result ); //$NON-NLS-1$
    return observation;
  }

  public SinuositaetProfileObject( final IObservation<TupleResult> observation )
  {
    super( observation );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfileObject#getId()
   */
  @Override
  public String getId( )
  {
    return ID;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.AbstractProfileObject#getProfileProperties()
   */
  @Override
  protected String[] getProfileProperties( )
  {
    return new String[] {};
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.ISinuositaetProfileObject#getKennung()
   */
  @Override
  public SINUOSITAET_KENNUNG getKennung( )
  {
    final IObservation<TupleResult> observation = getObservation();
    final TupleResult result = observation.getResult();
    if( result.isEmpty() )
      return null;

    final int index = result.indexOfComponent( PROPERTY_KENNUNG );
    if( index < 0 )
      return null;
    final IRecord record = result.get( 0 );
    return SINUOSITAET_KENNUNG.valueOf( record.getValue( index ).toString() );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.ISinuositaetProfileObject#getSinuositaet()
   */
  @Override
  public Double getSinuositaet( )
  {
    final IObservation<TupleResult> observation = getObservation();
    final TupleResult result = observation.getResult();
    if( result.isEmpty() )
      return null;

    final int index = result.indexOfComponent( PROPERTY_SN );

    final IRecord record = result.get( 0 );
    return (Double) record.getValue( index );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.ISinuositaetProfileObject#getGerinneArt()
   */
  @Override
  public SINUOSITAET_GERINNE_ART getGerinneArt( )
  {
    final IObservation<TupleResult> observation = getObservation();
    final TupleResult result = observation.getResult();
    if( result.isEmpty() )
      return null;

    final int index = result.indexOfComponent( PROPERTY_GERINNE_ART );

    final IRecord record = result.get( 0 );
    return SINUOSITAET_GERINNE_ART.valueOf( record.getValue( index ).toString() );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.ISinuositaetProfileObject#getLf()
   */
  @Override
  public Double getLf( )
  {
    final IObservation<TupleResult> observation = getObservation();
    final TupleResult result = observation.getResult();
    if( result.isEmpty() )
      return null;

    final int index = result.indexOfComponent( PROPERTY_LF );

    final IRecord record = result.get( 0 );
    return (Double) record.getValue( index );

  }
}
