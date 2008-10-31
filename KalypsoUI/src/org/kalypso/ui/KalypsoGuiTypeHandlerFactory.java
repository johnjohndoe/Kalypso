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
package org.kalypso.ui;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.PlatformUI;
import org.kalypso.gmlschema.types.ITypeHandlerFactory;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.gui.GuiTypeHandlerUtilities;
import org.kalypso.ogc.gml.gui.IGuiTypeHandler;
import org.kalypso.ogc.gml.gui.TimeseriesLinkGuiTypeHandler;
import org.kalypso.ogc.gml.gui.ZmlInlineGuiTypeHandler;
import org.kalypso.ogc.gml.typehandler.ZmlInlineTypeHandler;
import org.kalypso.ogc.sensor.IObservation;

/**
 * @author kurzbach
 */
public class KalypsoGuiTypeHandlerFactory implements ITypeHandlerFactory<IGuiTypeHandler>
{
  /**
   * @see org.kalypso.gmlschema.types.ITypeHandlerFactory#registerTypeHandlers(org.kalypso.gmlschema.types.ITypeRegistry)
   */
  public void registerTypeHandlers( final ITypeRegistry<IGuiTypeHandler> guiRegistry )
  {
    try
    {
      final ZmlInlineTypeHandler wvqInline = new ZmlInlineTypeHandler( "ZmlInlineWVQType", ZmlInlineTypeHandler.WVQ.axis, IObservation.class ); //$NON-NLS-1$
      final ZmlInlineTypeHandler taInline = new ZmlInlineTypeHandler( "ZmlInlineTAType", ZmlInlineTypeHandler.TA.axis, IObservation.class ); //$NON-NLS-1$
      final ZmlInlineTypeHandler wtKcLaiInline = new ZmlInlineTypeHandler( "ZmlInlineIdealKcWtLaiType", ZmlInlineTypeHandler.WtKcLai.axis, IObservation.class ); //$NON-NLS-1$
      final ZmlInlineTypeHandler tnInline = new ZmlInlineTypeHandler( "ZmlInlineTNType", ZmlInlineTypeHandler.TN.axis, IObservation.class ); //$NON-NLS-1$

      GuiTypeHandlerUtilities.registerXSDSimpleTypeHandler( guiRegistry );
      guiRegistry.registerTypeHandler( new ZmlInlineGuiTypeHandler( wvqInline ) );
      guiRegistry.registerTypeHandler( new ZmlInlineGuiTypeHandler( taInline ) );
      guiRegistry.registerTypeHandler( new ZmlInlineGuiTypeHandler( wtKcLaiInline ) );
      guiRegistry.registerTypeHandler( new ZmlInlineGuiTypeHandler( tnInline ) );
      guiRegistry.registerTypeHandler( new TimeseriesLinkGuiTypeHandler() );
    }
    catch( final Exception e ) // generic exception caught for simplicity
    {
      e.printStackTrace();
      // this method is also used in headless mode
      if( PlatformUI.isWorkbenchRunning() )
      {
        MessageDialog.openError( PlatformUI.getWorkbench().getDisplay().getActiveShell(), Messages.getString("org.kalypso.ui.KalypsoGuiTypeHandlerFactory.4"), e.getLocalizedMessage() ); //$NON-NLS-1$
      }
    }
  }
}
