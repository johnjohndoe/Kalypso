package ejb.event;

public class Client implements EJBEventListener {

    /** Creates new Client */
    public Client() {
    }
    
    public void init() {
        EJBEventManager em = EJBEventManager.getInstance();
        em.addEJBEventListener(this);
    }
    
    public void shutdown() {
        EJBEventManager em = EJBEventManager.getInstance();
        em.removeEJBEventListener(this);
    }

    public void notify(EJBEvent event) {
        switch(event.getEventType()) {
            case 0: break;
            case 1: break;
        }
    }
    
    
    public static void main(String[] args) {
        Client c = new Client();
    }
    
}