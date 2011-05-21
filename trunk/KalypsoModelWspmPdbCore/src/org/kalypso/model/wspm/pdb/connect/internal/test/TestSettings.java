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
package org.kalypso.model.wspm.pdb.connect.internal.test;

import org.eclipse.core.databinding.DataBindingContext;
import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.IPdbSettings;
import org.kalypso.model.wspm.pdb.connect.IPdbSettingsControl;
import org.kalypso.model.wspm.pdb.connect.internal.AbstractSettings;

/**
 * @author Gernot Belger
 */
public class TestSettings extends AbstractSettings
{
  public static final String TYPE = "test"; //$NON-NLS-1$

  public TestSettings( )
  {
    this( "Test" ); //$NON-NLS-1$;
  }

  private TestSettings( final String name )
  {
    super( name );
  }

  @Override
  public String getType( )
  {
    return TYPE;
  }

  @Override
  public ImageDescriptor getImage( )
  {
    return null;
  }

  @Override
  public IPdbConnection createConnection( )
  {
    return new TestConnection( this );
  }

  @Override
  public void saveState( final ISecurePreferences preferences )
  {
  }

  @Override
  public void readState( final ISecurePreferences preferences )
  {
  }

  @Override
  public IPdbSettings copy( )
  {
    return new TestSettings( getName() );
  }

  @Override
  public IPdbSettingsControl createEditControl( final DataBindingContext binding, final Composite parent )
  {
    return new TestSettingsControl( binding, parent, this );
  }

  @Override
  public String toString( )
  {
    return String.format( "%s (for debugging purpose only)", getName() );
  }
}