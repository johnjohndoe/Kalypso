
package org.kalypso.services.user.impl.jaxws;

import javax.xml.bind.annotation.AccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import org.kalypso.services.user.impl.jaxws.IsAskForLoginResponse;

@XmlRootElement(name = "isAskForLoginResponse", namespace = "http://impl.user.services.kalypso.org/")
@XmlAccessorType(AccessType.FIELD)
@XmlType(name = "isAskForLoginResponse", namespace = "http://impl.user.services.kalypso.org/")
public class IsAskForLoginResponse {

    @XmlElement(name = "return", namespace = "")
    private boolean _return;

    /**
     * 
     * @return
     *     returns boolean
     */
    public boolean is_return() {
        return this._return;
    }

    /**
     * 
     * @param _return
     *     the value for the _return property
     */
    public void set_return(boolean _return) {
        this._return = _return;
    }

}
