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
package org.kalypso.ui.rrm.internal.cm.view;

import org.eclipse.core.databinding.validation.IValidator;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.rcm.binding.ILinearSumGenerator;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;

/**
 * The timestamp validator.
 * 
 * @author Holger Albert
 */
public class TimestampValidator implements IValidator
{
  /**
   * The feature bean.
   */
  private final FeatureBean<ILinearSumGenerator> m_featureBean;

  /**
   * The constructor.
   * 
   * @param featureBean
   *          The feature bean.
   */
  public TimestampValidator( final FeatureBean<ILinearSumGenerator> featureBean )
  {
    m_featureBean = featureBean;
  }

  /**
   * @see org.eclipse.core.databinding.validation.IValidator#validate(java.lang.Object)
   */
  @Override
  public IStatus validate( final Object value )
  {
    /* Get the timestep. */
    final Integer timestep = (Integer) m_featureBean.getProperty( ILinearSumGenerator.PROPERTY_TIMESTEP );
    if( timestep != null && timestep.intValue() == 1440 )
    {
      /* We need a valid timestamp, if the timestep represents one day (1440 minutes). */
      /* The case that the timestamp format is wrong is covered in the above if. */
      final String timestamp = (String) value;
      if( timestamp == null || timestamp.length() == 0 )
        return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "For day values, a timestamp is needed..." );
    }

    return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), "OK" );
  }
}