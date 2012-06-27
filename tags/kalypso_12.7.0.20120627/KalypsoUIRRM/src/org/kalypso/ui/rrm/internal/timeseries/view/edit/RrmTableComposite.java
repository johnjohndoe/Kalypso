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
package org.kalypso.ui.rrm.internal.timeseries.view.edit;

import java.net.URL;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.services.IServiceLocator;
import org.kalypso.zml.ui.table.IZmlTable;
import org.kalypso.zml.ui.table.context.IZmlTableSource;
import org.kalypso.zml.ui.table.context.TableSourceProvider;
import org.kalypso.zml.ui.table.view.TableComposite;

/**
 * @author Dirk Kuch
 */
public class RrmTableComposite extends TableComposite
{

  private final IServiceLocator m_context;

  private TableSourceProvider m_provider;

  public RrmTableComposite( final Composite parent, final FormToolkit toolkit, final URL template, final IServiceLocator context )
  {
    super( parent, toolkit, template );
    m_context = context;

    activate( m_context );
  }

  protected void activate( final IServiceLocator context )
  {
    m_provider = new TableSourceProvider( context, new IZmlTableSource()
    {
      @Override
      public IZmlTable getTable( )
      {
        return RrmTableComposite.this.getTable();
      }

      @Override
      public org.kalypso.zml.ui.table.IZmlTableComposite getComposite( )
      {
        return getTableComposite();
      }
    } );
  }

  public void deactivate( )
  {
    m_provider.dispose();
  }
}
