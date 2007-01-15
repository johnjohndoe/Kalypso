/*******************************************************************************
 * Copyright (c) 2005, 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.kalypso.kalypsosimulationmodel.wizard.shapeImport;

import java.text.MessageFormat;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * 
 * @author huebsch
 */
public class WizardMessages
{
  private static final String BUNDLE_NAME = "org.kalypso.wizard.WizardMessages";//$NON-NLS-1$

  private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle( BUNDLE_NAME );

  private WizardMessages()
  {}

  /**
   * @param key -
   *          zu übersetzender Schlüssel
   * @return - die übersetzte Nachricht
   */
  public static String getString( String key )
  {
    try
    {
      return RESOURCE_BUNDLE.getString( key );
    }
    catch( MissingResourceException e )
    {
      return '!' + key + '!';
    }
  }

  /**
   * @param key -
   *          zu übersetzender Schlüssel
   * @param params -
   *          einzufügende Parameter
   * @return - die übersetzte Nachricht
   */
  public static String getString( String key, Object[] params )
  {
    if( params == null )
      return getString( key );
    try
    {
      return MessageFormat.format( getString( key ), params );
    }
    catch( Exception e )
    {
      return '!' + key + '!';
    }
  }
}
