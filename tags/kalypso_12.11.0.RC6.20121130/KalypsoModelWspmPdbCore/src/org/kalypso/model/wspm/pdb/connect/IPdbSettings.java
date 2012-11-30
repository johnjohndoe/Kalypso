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
package org.kalypso.model.wspm.pdb.connect;

import org.eclipse.core.databinding.DataBindingContext;
import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.equinox.security.storage.StorageException;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Composite;

/**
 * Represents the settings needed to connect to a pdb.
 * 
 * @author Gernot Belger
 */
public interface IPdbSettings extends Comparable<IPdbSettings>
{
  String getType( );

  /**
   * (Unique) name of this settings.
   */
  String getName( );

  String getUsername( );

  ImageDescriptor getImage( );

  IPdbConnection createConnection( );

  /**
   * Saves the state of this connection settings into the given {@link IMemento}.<br/>
   */
  void saveState( ISecurePreferences preferences ) throws StorageException;

  /**
   * Red the state of this settings from the given preferences.
   */
  void readState( ISecurePreferences preferences ) throws StorageException;

  /**
   * Crates a copy of this instance.
   */
  IPdbSettings copy( );

  /**
   * Create a control that will edit the parameters of this instance.
   */
  IPdbSettingsControl createEditControl( DataBindingContext binding, Composite parent );
}