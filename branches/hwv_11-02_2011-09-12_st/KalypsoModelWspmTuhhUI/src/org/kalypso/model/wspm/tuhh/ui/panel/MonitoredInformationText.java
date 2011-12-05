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
package org.kalypso.model.wspm.tuhh.ui.panel;

import java.math.BigDecimal;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.IMessageManager;
import org.eclipse.ui.forms.ManagedForm;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.validation.BigDecimalParser;
import org.kalypso.commons.validation.IFormValidationRule;
import org.kalypso.commons.validation.IValueReceiver;
import org.kalypso.commons.validation.NotNullError;
import org.kalypso.commons.validation.RuleValidator;
import org.kalypso.commons.validation.ValidatingModifyListener;
import org.kalypso.model.wspm.core.gml.IProfileFeature;

/**
 * @author kimwerner
 */
public class MonitoredInformationText
{
  public MonitoredInformationText( final Composite parent, final ManagedForm managedForm )
  {

    final FormToolkit toolkit = managedForm.getToolkit();
    m_messageManager = managedForm.getMessageManager();

    m_text = toolkit.createText( parent, "" ); //$NON-NLS-1$

    m_modifyListener = new ValidatingModifyListener( m_text, new BigDecimalParser( true, IProfileFeature.STATION_SCALE ), m_messageManager );
    m_modifyListener.setValueReceiver( m_receiver );
    addRule( new NotNullError() );
  }

  private final IMessageManager m_messageManager;

  private final Text m_text;

  final IValueReceiver m_receiver = new IValueReceiver()
  {
    @Override
    public void updateValue( final Object object )
    {
      final BigDecimal big = (BigDecimal) object;

      updateText( big );
    }
  };

  private final RuleValidator m_ruleValidator = new RuleValidator();

  private final ValidatingModifyListener m_modifyListener;

  public void addRule( IFormValidationRule rule )
  {
    m_ruleValidator.addRule( rule );
  }

  protected void updateText( final BigDecimal big )
  {
    if( m_text.isDisposed() )
      return;
    if( big == null )
      return;// m_text.setText( "null" );
    else
      m_text.setText( big.toString() );
  }
}
