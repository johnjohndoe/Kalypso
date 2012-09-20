/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.tuhh.ui.panel.energyloss;

import java.math.BigDecimal;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.changes.ProfileObjectEdit;
import org.kalypso.model.wspm.core.profil.operation.ProfileOperation;
import org.kalypso.model.wspm.core.profil.operation.ProfileOperationJob;
import org.kalypso.model.wspm.tuhh.core.profile.energyloss.EnergylossProfileObject;
import org.kalypso.model.wspm.tuhh.core.profile.energyloss.IEnergylossProfileObject;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

/**
 * @author kimwerner
 */
public class EnergylossDataModel extends AbstractModelObject
{
  public static final String PROPERTY_ENERGYLOSS_VALUE = "energylossValue"; //$NON-NLS-1$

  public static final String PROPERTY_ENERGYLOSS_TYPE = "energylossType"; //$NON-NLS-1$

  private final IEnergylossProfileObject m_energylossObject;

  private final IProfile m_profile;

  private final int m_index;

  private BigDecimal m_value;

  private String m_type;

  public EnergylossDataModel( final IProfile profile, final int index )
  {
    m_profile = profile;
    IEnergylossProfileObject[] obs = profile.getProfileObjects( EnergylossProfileObject.class );
    m_energylossObject = obs.length > 0 ? obs[0] : null;
    m_index = index;
  }

  public void fireObjectChange( String type, BigDecimal newValue )
  {
    if( type == null || newValue == null )
    {
      return;
    }

    final IComponent cmpVal = m_energylossObject.getObjectProperty( IEnergylossProfileObject.PROPERTY_VALUE );
    final IComponent cmpTyp = m_energylossObject.getObjectProperty( IEnergylossProfileObject.PROPERTY_TYPE );
    final IRecord rec = getRecord();
    int index = m_index;
    if( rec == null )
    {
      final TupleResult result = m_energylossObject.getObservation().getResult();
      index = result.size();
      final IRecord newRec = m_energylossObject.getObservation().getResult().createRecord();
      result.add( newRec );
    }
    final ProfileObjectEdit changes = new ProfileObjectEdit( m_energylossObject, cmpVal, index, newValue.doubleValue() );
    final ProfileOperation operation = new ProfileOperation( "updating energyloss", m_profile, changes, true ); //$NON-NLS-1$
    operation.addChange( new ProfileObjectEdit( m_energylossObject, cmpTyp, index, type ) );
    new ProfileOperationJob( operation ).schedule();
  }

  public String getEnergylossType( )
  {
    if( m_type == null )
    {
      final IRecord rec = getRecord();
      if( rec == null )
      {
        return null;
      }
      final int iType = rec.indexOfComponent( IEnergylossProfileObject.PROPERTY_TYPE );
      final Object objTyp = rec.getValue( iType );
      m_type = objTyp == null ? null : objTyp.toString();
    }
    return m_type;
  }

  public BigDecimal getEnergylossValue( )
  {
    if( m_value == null )
    {
      final IRecord rec = getRecord();
      if( rec == null )
      {
        return null;
      }
      // final int iType = rec.indexOfComponent( IEnergylossProfileObject.PROPERTY_TYPE );
      final int iValue = rec.indexOfComponent( IEnergylossProfileObject.PROPERTY_VALUE );
      final Object objVal = rec.getValue( iValue );
      // final Object objTyp = rec.getValue( iType );
      m_value = objVal == null ? null : new BigDecimal( (Double) objVal );
      // return new Pair<String, BigDecimal>( objTyp.toString(), energylossValue );
    }
    return m_value;
  }

  public IObservableValue getObservableValue( final String property )
  {
    return BeansObservables.observeValue( this, property );
  }

  private final IRecord getRecord( )
  {
    final TupleResult result = m_energylossObject.getObservation().getResult();
    if( m_index < result.size() )
    {
      return result.get( m_index );
    }
    return null;
  }

  public void removeEnergyloss( )
  {
    final TupleResult result = m_energylossObject.getObservation().getResult();
    if( m_index < result.size() )
    {
      result.remove( m_index );
    }
  }

  public void setEnergylossType( String energylossType )
  {
    m_type = energylossType;
  }

  public void setEnergylossValue( BigDecimal energylossValue )
  {
    m_value = energylossValue;
    fireObjectChange( getEnergylossType(), energylossValue );
  }
}
