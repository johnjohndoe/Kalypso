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
package org.kalypso.model.wspm.pdb.internal.update;

import java.io.File;

import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.databinding.observable.value.IValueChangeListener;
import org.eclipse.core.databinding.observable.value.ValueChangeEvent;
import org.eclipse.jface.action.Action;
import org.eclipse.swt.program.Program;
import org.eclipse.swt.widgets.Event;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class TestDemBaseAction extends Action
{
  private final IObservableValue m_fileValue;

  public TestDemBaseAction( final IObservableValue fileValue )
  {
    m_fileValue = fileValue;

    setText( Messages.getString("TestDemBaseAction_0") ); //$NON-NLS-1$
    setToolTipText( Messages.getString("TestDemBaseAction_1") ); //$NON-NLS-1$

    m_fileValue.addValueChangeListener( new IValueChangeListener()
    {
      @Override
      public void handleValueChange( final ValueChangeEvent event )
      {
        update();
      }
    } );

    update();
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final File dir = getDirectory();
    if( dir == null )
      return;

    Program.launch( dir.getAbsolutePath() );
  }

  private File getDirectory( )
  {
    return (File) m_fileValue.getValue();
  }

  protected void update( )
  {
    final File dir = getDirectory();
    setEnabled( dir != null && dir.isDirectory() );
  }
}