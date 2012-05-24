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
package org.kalypso.ui.rrm.internal.utils.featureTree;

import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.contribs.eclipse.swt.widgets.SectionUtils;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public abstract class AbstractTreeNodeUiHandler implements ITreeNodeUiHandler
{
  @Override
  public final Control createControl( final Composite parent, final IDataBinding binding )
  {
    final FormToolkit toolkit = binding.getToolkit();

    final Composite panel = toolkit.createComposite( parent );
    GridLayoutFactory.fillDefaults().numColumns( 2 ).spacing( 0, 0 ).applyTo( panel );

    final Section controlSection = toolkit.createSection( panel, Section.TITLE_BAR | Section.EXPANDED );
    controlSection.setText( Messages.getString( "AbstractTreeNodeUiHandler_0" ) ); //$NON-NLS-1$
    controlSection.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    final ToolBarManager toolbar = SectionUtils.createSectionToolbar( controlSection );

    /* Properties Section */
    final Control propertiesControl = createPropertiesControl( controlSection, binding, toolbar );
    if( propertiesControl == null )
      controlSection.dispose();
    else
    {
      controlSection.setClient( propertiesControl );
      toolbar.update( true );
    }

    /* Action section */
    final Section actionSection = toolkit.createSection( panel, Section.TITLE_BAR | Section.EXPANDED );
    actionSection.setText( Messages.getString( "AbstractTreeNodeUiHandler_1" ) ); //$NON-NLS-1$
    actionSection.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    final Composite actionPanel = toolkit.createComposite( actionSection );
    GridLayoutFactory.fillDefaults().applyTo( actionPanel );
    actionSection.setClient( actionPanel );

    createHyperlinks( toolkit, actionPanel );

    if( actionPanel.getChildren().length == 0 )
      actionSection.dispose();

    return panel;
  }

  protected abstract Control createPropertiesControl( Composite parent, IDataBinding binding, ToolBarManager sectionToolbar );

  protected abstract void createHyperlinks( FormToolkit toolkit, Composite actionPanel );

  @Override
  public int compareTo( final ITreeNodeUiHandler other )
  {
    if( other instanceof AbstractTreeNodeUiHandler )
    {
      final AbstractTreeNodeUiHandler o = (AbstractTreeNodeUiHandler) other;
      return getTreeCompareLabel().compareTo( o.getTreeCompareLabel() );
    }

    return getTreeLabel().compareTo( other.getTreeLabel() );
  }

  protected String getTreeCompareLabel( )
  {
    return getTreeLabel();
  }
}
