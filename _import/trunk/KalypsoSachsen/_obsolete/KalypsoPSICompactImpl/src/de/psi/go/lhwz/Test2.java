/*
 * Created on 19.05.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package de.psi.go.lhwz;

import de.psi.go.lhwz.PSICompact.ObjectInfo;

/**
 * @author BKarpa
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class Test2 {
    public static void main(String[] args) {
        Test t = new Test();
        try {
            ObjectInfo[] o = t.getInfo(de.psi.go.lhwz.PSICompact.TYPE_MEASUREMENT);
            for (int i = 0; i < o.length; i++) {
                System.out.println("o["+i+"].id: " + o[i].getId());
                System.out.println("o["+i+"].Beschreibung: " + o[i].getDescription());
            }
            
            
        } catch (ECommException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
}
