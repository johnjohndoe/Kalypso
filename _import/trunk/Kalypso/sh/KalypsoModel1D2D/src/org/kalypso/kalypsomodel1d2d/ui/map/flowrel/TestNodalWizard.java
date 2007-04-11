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
package org.kalypso.kalypsomodel1d2d.ui.map.flowrel;

import junit.framework.TestCase;

import org.eclipse.jface.wizard.WizardDialog;
import org.kalypso.kalypsomodel1d2d.ui.map.flowrel.CreateNodalBCFlowrelationWidget.TimeserieTypeDescription;

import test.org.kalypso.kalypsosimulationmodel.TestUtils;

/**
 * @author antanas
 *
 */
public class TestNodalWizard extends TestCase
{

  /**
   * Test method for {@link org.kalypso.kalypsomodel1d2d.ui.map.flowrel.NodalBCSelectionWizard#addPages()}.
   */
  public final void testAddPages( )
  {
    final TimeserieTypeDescription[] descriptions = new TimeserieTypeDescription[] {
        new TimeserieTypeDescription( "Wasserstand - Zeitreihe", "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time", "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Waterlevel" ),
        new TimeserieTypeDescription( "Abfluss - Zeitreihe", "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time", "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Discharge" ),
        new TimeserieTypeDescription( "Spezifische Abfluss - Zeitreihe", "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time", "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#SpecificDischarge1D" ),
        new TimeserieTypeDescription( "W/Q - Beziehung", "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Waterlevel", "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Discharge" ) };

      try
      {
        NodalBCSelectionWizard wizard = new NodalBCSelectionWizard(descriptions, null, null, null);
        WizardDialog dialog = new WizardDialog( null, wizard );
        dialog.create();
        dialog.open();
      }
      catch(Throwable th)
      {
        fail(TestUtils.getStackTraceAsString( th ));
      }

    }

}
