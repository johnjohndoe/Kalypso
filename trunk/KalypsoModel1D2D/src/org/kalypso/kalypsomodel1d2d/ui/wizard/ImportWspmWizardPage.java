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
package org.kalypso.kalypsomodel1d2d.ui.wizard;

import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.util.Arrays;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.ui.editor.gmleditor.ui.GMLLabelProvider;
import org.kalypso.util.swt.ListSelectionWizardPage;

/**
 * @author Gernot Belger
 */
public class ImportWspmWizardPage extends ListSelectionWizardPage
{
  private TuhhCalculation m_calculation;

  public ImportWspmWizardPage( final String pageName )
  {
    super( pageName, new GMLLabelProvider() );

    setTitle( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.ImportWspmWizardPage.0" ) ); //$NON-NLS-1$
    setMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.ImportWspmWizardPage.1" ) ); //$NON-NLS-1$
  }

  public TuhhCalculation getCalculation( )
  {
    return m_calculation;
  }

  public void setCalculation( final TuhhCalculation calculation )
  {
    m_calculation = calculation;

    final List< ? > reaches = (List< ? >) m_calculation.getReach().getProperty( TuhhReach.QNAME_PROP_REACHSEGMENTMEMBER );
    setInput( reaches );

    setCheckedElements( reaches.toArray( new Object[reaches.size()] ) );

    setStatus( StatusUtilities.createStatus( IStatus.INFO, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.ImportWspmWizardPage.2" ) + calculation.getName(), null ) ); //$NON-NLS-1$
  }

  public TuhhReachProfileSegment[] getReachProfileSegments( )
  {
    final Object[] selection = getSelection();
    return Arrays.castArray( selection, new TuhhReachProfileSegment[selection.length] );
  }

}
