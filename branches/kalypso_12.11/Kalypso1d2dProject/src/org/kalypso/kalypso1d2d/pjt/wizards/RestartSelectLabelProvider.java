/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.kalypso1d2d.pjt.wizards;

import org.eclipse.core.databinding.observable.list.IListChangeListener;
import org.eclipse.core.databinding.observable.list.ListChangeEvent;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.viewers.IFontProvider;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.LabelProviderChangedEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.model.WorkbenchLabelProvider;

/**
 * @author Gernot Belger
 */
public class RestartSelectLabelProvider extends LabelProvider implements IFontProvider
{
  private final ILabelProvider m_delegate = WorkbenchLabelProvider.getDecoratingWorkbenchLabelProvider();

  private final IListChangeListener m_listListener = new IListChangeListener()
  {
    @Override
    public void handleListChange( final ListChangeEvent event )
    {
      updateElements();
    }
  };

  private final RestartSelectData m_data;

  public RestartSelectLabelProvider( final RestartSelectData data )
  {
    m_data = data;

    m_data.getRestartResultSet().addListChangeListener( m_listListener );
  }

  protected void updateElements( )
  {
    final LabelProviderChangedEvent event = new LabelProviderChangedEvent( this );
    fireLabelProviderChanged( event );
  }

  @Override
  public void dispose( )
  {
    super.dispose();

    m_delegate.dispose();
  }

  @Override
  public String getText( final Object element )
  {
    return m_delegate.getText( element );
  }

  @Override
  public Image getImage( final Object element )
  {
    return m_delegate.getImage( element );
  }

  @Override
  public Font getFont( final Object element )
  {
    final boolean isRestart = m_data.getRestartResultSet().contains( element );
    if( isRestart )
      return JFaceResources.getBannerFont();

    return null;
  }
}