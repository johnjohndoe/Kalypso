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
package org.kalypso.ui.rrm.internal.results.view.tree.handlers;

import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.contribs.eclipse.jface.action.ActionHyperlink;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ui.rrm.internal.results.view.actions.OpenResultTimeseriesTableAction;
import org.kalypso.ui.rrm.internal.results.view.base.IHydrologyResultReference;
import org.kalypso.ui.rrm.internal.results.view.base.KalypsoHydrologyResults.RRM_RESULT;

/**
 * @author Dirk Kuch
 */
public class HydrologyResultReferenceUiHandler extends AbstractResultTreeNodeUiHandler
{

  private final IHydrologyResultReference m_reference;

  public HydrologyResultReferenceUiHandler( final RrmSimulation simulation, final IHydrologyResultReference reference )
  {
    super( simulation );
    m_reference = reference;
  }

  @Override
  public String getTreeLabel( )
  {
    return m_reference.getType().getLabel();
  }

  @Override
  public ImageDescriptor getTreeImage( )
  {
    final RRM_RESULT type = m_reference.getType();

    if( m_reference.isValid() )
      return type.getImage();

    return type.getMissingImage();
  }

  @Override
  protected Control createPropertiesControl( final Composite parent, final IDataBinding binding, final ToolBarManager sectionToolbar )
  {
    return null;
  }

  @Override
  protected void createHyperlinks( final FormToolkit toolkit, final Composite actionPanel )
  {

    try
    {

      final URL url = m_reference.getUrl();
      final OpenResultTimeseriesTableAction actionZmlTable = new OpenResultTimeseriesTableAction( url );

      final ImageHyperlink imageHyperlink = ActionHyperlink.createHyperlink( null, actionPanel, SWT.NONE, actionZmlTable );
      imageHyperlink.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
      imageHyperlink.setText( actionZmlTable.getText() );
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
    }

    super.createHyperlinks( toolkit, actionPanel );
  }

}
