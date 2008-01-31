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
package org.kalypso.core;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.PlatformUI;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeHandlerFactory;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.ogc.gml.typehandler.ResourceFileTypeHandler;
import org.kalypso.ogc.gml.typehandler.ZmlInlineTypeHandler;
import org.kalypso.ogc.sensor.IObservation;

/**
 * @author kurzbach
 */
public class KalypsoCoreTypeHandlerFactory implements ITypeHandlerFactory<IMarshallingTypeHandler>
{

  /**
   * @see org.kalypso.gmlschema.types.ITypeHandlerFactory#registerTypeHandlers(org.kalypso.gmlschema.types.ITypeRegistry)
   */
  public void registerTypeHandlers( final ITypeRegistry<IMarshallingTypeHandler> marshallingRegistry )
  {
    try
    {
      final ZmlInlineTypeHandler wvqInline = new ZmlInlineTypeHandler( "ZmlInlineWVQType", ZmlInlineTypeHandler.WVQ.axis, IObservation.class );
      final ZmlInlineTypeHandler taInline = new ZmlInlineTypeHandler( "ZmlInlineTAType", ZmlInlineTypeHandler.TA.axis, IObservation.class );
      final ZmlInlineTypeHandler wtKcLaiInline = new ZmlInlineTypeHandler( "ZmlInlineIdealKcWtLaiType", ZmlInlineTypeHandler.WtKcLai.axis, IObservation.class );
      final ZmlInlineTypeHandler tnInline = new ZmlInlineTypeHandler( "ZmlInlineTNType", ZmlInlineTypeHandler.TN.axis, IObservation.class );
      final ResourceFileTypeHandler resourceFileTypeHandler = new ResourceFileTypeHandler();

      marshallingRegistry.registerTypeHandler( resourceFileTypeHandler );
      marshallingRegistry.registerTypeHandler( wvqInline );
      marshallingRegistry.registerTypeHandler( taInline );
      marshallingRegistry.registerTypeHandler( wtKcLaiInline );
      marshallingRegistry.registerTypeHandler( tnInline );
    }
    catch( final Exception e ) // generic exception caught for simplicity
    {
      e.printStackTrace();
      // this method is also used in headless mode
      if( PlatformUI.isWorkbenchRunning() )
      {
        MessageDialog.openError( PlatformUI.getWorkbench().getDisplay().getActiveShell(), "Interne Applikationsfehler", e.getLocalizedMessage() );
      }
    }
  }
}
