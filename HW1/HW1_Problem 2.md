$$
\epsilon_t=\mu+\epsilon_{t-1}+\eta_t=\mu+\mu+\epsilon_{t-2}+\eta_{t-1}+\eta_t=t\mu+\epsilon_0+\eta_1+...+\eta_t=t\mu+\epsilon_0+\sum_{i=1}^t \eta_i\\
\epsilon_{t-1}=(t-1)\mu+\epsilon_0+\sum_{i=1}^{t-1} \eta_i\\
...\\
\epsilon_2=2\mu+\epsilon_0+\eta_1+\eta_2\\
\epsilon_1=\mu+\epsilon_0+\eta_1
$$
---
$$
r_t=r_{t-1}+\epsilon_t=r_{t-1}+t\mu+\epsilon_0+\sum_{i=1}^t \eta_i\\
r_{t-1}=r_{t-2}+(t-1)\mu+\epsilon_0+\sum_{i=1}^{t-1} \eta_i\\
...\\
r_2=r_1+2\mu+\epsilon_0+\eta_1+\eta_2\\
r_1=r_0+\mu+\epsilon_0+\eta_1
$$
---
$$
r_t=r_0+\mu+\epsilon_0+\eta_1+2\mu+\epsilon_0+\eta_1+\eta_2+...+(t-1)\mu+\epsilon_0+\sum_{i=1}^{t-1} \eta_i+t\mu+\epsilon_0+\sum_{i=1}^t \eta_i=\\=r_0+\mu\sum_{i=1}^t i+t\epsilon_0+t\eta_1+(t-1)\eta_2+...+\eta_t=r_0+\mu\sum_{i=1}^t i+t\epsilon_0+\sum_{i=1}^t (t+1-i)\eta_i=\\=r_0+\mu\frac{t(t+1)}{2}+t\epsilon_0+\sum_{i=1}^t (t+1-i)\eta_i
$$
---
$$
E(r_t)=E[r_0+\mu\frac{t(t+1)}{2}+t\epsilon_0+\sum_{i=1}^t (t+1-i)\eta_i]=\\=E(r_0)+E[\mu\frac{t(t+1)}{2}]+E(t\epsilon_0)+E[\sum_{i=1}^t (t+1-i)\eta_i]=\\=r_0+\mu\frac{t(t+1)}{2}+t\epsilon_0+\sum_{i=1}^t (t+1-i)E(\eta_i)=\\=r_0+\mu\frac{t(t+1)}{2}+t\epsilon_0
$$
---
$$
Var(r_t)=E(r_t^2)-[E(r_t)]^2=E\{[r_0+\mu\frac{t(t+1)}{2}+t\epsilon_0+\sum_{i=1}^t (t+1-i)\eta_i]^2\}-[r_0+\mu\frac{t(t+1)}{2}+t\epsilon_0]^2=\\=E[r_0^2+r_0\mu\frac{t(t+1)}{2}+r_0t\epsilon_0+r_0\sum_{i=1}^t (t+1-i)\eta_i+r_0\mu\frac{t(t+1)}{2}+\mu^2\frac{t^2(t+1)^2}{4}+\mu\frac{t(t+1)}{2}t\epsilon_0+\\+\mu\frac{t(t+1)}{2}\sum_{i=1}^t (t+1-i)\eta_i+r_0t\epsilon_0+t\epsilon_0\mu\frac{t(t+1)}{2}+t^2\epsilon_0^2+t\epsilon_0\sum_{i=1}^t (t+1-i)\eta_i+r_0\sum_{i=1}^t (t+1-i)\eta_i+\mu\frac{t(t+1)}{2}\sum_{i=1}^t (t+1-i)\eta_i+\\+t\epsilon_0\sum_{i=1}^t (t+1-i)\eta_i+(\sum_{i=1}^t (t+1-i)\eta_i)^2]-[r_0^2+r_0\mu\frac{t(t+1)}{2}+r_0t\epsilon_0+r_0\mu\frac{t(t+1)}{2}+\mu^2\frac{t^2(t+1)^2}{4}+\\+t\epsilon_0\mu\frac{t(t+1)}{2}+r_0t\epsilon_0+t\epsilon_0\mu\frac{t(t+1)}{2}+t^2\epsilon_0^2]=r_0^2+r_0\mu t(t+1)+2r_0t\epsilon_0+\mu^2\frac{t^2(t+1)^2}{4}+\mu t(t+1)t\epsilon_0+\\+t^2\epsilon_0^2+E\{[\sum_{i=1}^t (t+1-i)\eta_i]^2\}-r_0^2-r_0\mu t(t+1)-2r_0t\epsilon_0-\mu^2\frac{t^2(t+1)^2}{4}-t\epsilon_0\mu t(t+1)-t^2\epsilon_0^2=\\=E\{[\sum_{i=1}^t (t+1-i)\eta_i]^2\}=E[(t\eta_1+(t-1)\eta_2+...+\eta_t)^2]=E(t^2\eta_1^2)+E[(t-1)^2\eta_2^2]+...+E(\eta_t^2)=\\=t^2+(t-1)^2+..+1=\sum_{i=1}^t (t+1-i)^2
$$
---
$$
Cov(\epsilon_t,\epsilon_{t-1})=E(\epsilon_t\epsilon_{t-1})-E(\epsilon_t)E(\epsilon_{t-1})=E\{[t\mu+\epsilon_0+\sum_{i=1}^t \eta_i][(t-1)\mu+\epsilon_0+\sum_{i=1}^{t-1} \eta_i]\}-\\-[t\mu+\epsilon_0][(t-1)\mu+\epsilon_0]=E[t(t-1)\mu^2+t\mu\epsilon_0+t\mu\sum_{i=1}^{t-1} \eta_i+(t-1)\mu\epsilon_0+\epsilon_0^2+\\+\epsilon_0\sum_{i=1}^{t-1} \eta_i+(t-1)\mu\sum_{i=1}^t \eta_i+\epsilon_0\sum_{i=1}^t \eta_i+\sum_{i=1}^t \eta_i\sum_{i=1}^{t-1} \eta_i]-[t(t-1)\mu^2+t\mu\epsilon_0+\epsilon_0(t-1)\mu+\epsilon_0^2]=\\=t(t-1)\mu^2+t\mu\epsilon_0+(t-1)\mu\epsilon_0+\epsilon_0^2+E(\sum_{i=1}^t \eta_i\sum_{i=1}^{t-1} \eta_i)-t(t-1)\mu^2-t\mu\epsilon_0-\epsilon_0(t-1)\mu-\epsilon_0^2=\\=E[(\eta_1+..+\eta_t)(\eta_1+..+\eta_{t-1})]=E(\eta_1^2)+..+E(\eta_{t-1}^2)=(t-1)\sigma_\eta^2=t-1
$$