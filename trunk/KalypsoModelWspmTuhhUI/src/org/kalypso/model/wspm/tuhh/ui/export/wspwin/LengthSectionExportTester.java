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
package org.kalypso.model.wspm.tuhh.ui.export.wspwin;

import org.eclipse.core.expressions.PropertyTester;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;

import de.openali.odysseus.chart.framework.view.IChartComposite;

/**
 * Tests if a chart can be exported as WspWin Length Section.
 * 
 * @author Gernot Belger
 */
public class LengthSectionExportTester extends PropertyTester
{
  public static final String PROPERTY_IS_LENGTH_SECTION = "isLengthSection"; //$NON-NLS-1$

  @Override
  public boolean test( final Object receiver, final String property, final Object[] args, final Object expectedValue )
  {
    if( !(receiver instanceof IChartComposite) )
      return false;

    final IChartComposite chart = (IChartComposite) receiver;

    if( PROPERTY_IS_LENGTH_SECTION.equals( property ) )
      return testIsLengthSection( chart );

    return false;
  }

  private boolean testIsLengthSection( final IChartComposite chart )
  {
    final IObservation<TupleResult> observation = LengthSectionExportHandler.getLSObservation( chart );
    return observation != null;
  }
}