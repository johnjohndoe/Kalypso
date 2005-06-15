/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.bce.eclipse.ui.dialogs;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;

public class ResourceSelectionValidator
{
  
  public ResourceSelectionValidator(){
    super();
  }

  /**
   * @see org.eclipse.ui.dialogs.ISelectionValidator#isValid(java.lang.Object)
   */
  public boolean isValid( Object selection )
  {
    boolean valid = false;
    if(selection!=null && selection instanceof IResource){
      if(selection instanceof IFile)
        valid = true;
    }
    return valid;
  }

}
