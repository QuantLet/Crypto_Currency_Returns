function [loadings,F,f2,eigval, eigvec] = factor_an_static(stats)
% Factor Model

% standardizing data
Rho=corrcoef(stats);
n1 = length(stats);
m  = mean(stats);
zz = (stats - repmat(m,n1,1))./repmat(sqrt(var(stats)), n1, 1);
Cor=corrcoef(zz);

%%
Rho=corrcoef(stats);
n1 = length(stats);
m  = mean(stats);
zz = (stats - repmat(m,n1,1))./repmat(sqrt(var(stats)), n1, 1);
Cor=corrcoef(zz);

[eigvec,eigval] = eigs(Cor);
o=ones(size(eigvec(:,1:3)));
egv=eigval(1:3,1:3);
E               = ones(size(eigvec(:,1:3)))*eigval(1:3,1:3);
Q     =sqrt(E).*eigvec(:,1:3);
%%
%% Principal Component Method with varimax rotation
% 
[eigvec,eigval] = eigs(Cor);
E               = ones(size(eigvec(:,1:3)))*eigval(1:3,1:3);

% the estimated factor loadings matrix - the initial factor pattern
Q     =sqrt(E).*eigvec(:,1:3);


%// Rotating loadings
% [ld, T] = rotatefactors(Q, 'method','varimax');
[ld, T] = varimax_manual(Q);

% T is the Orthogonal Transformation Matrix;
loadings=ld;
loadings(:,1)=ld(:,1);
loadings(:,2)=-ld(:,3);
loadings(:,3)= ld(:,2);
% f2 contains the standardized scoring coefficients;
f2=inv(Rho)*loadings;

% F contains the final scores after the varimax rotation;
F=zz*f2;



end

function [A_rot, R] = varimax_manual(A)

    % A: loadings matrix (p × k)

    eps = 1e-6;
    max_iter = 100;
    [p, k] = size(A);

    R = eye(k);

    for iter = 1:max_iter
        A_rot = A * R;
        Lambda = A_rot .^ 2;

        % varimax objective gradient
        B = A_rot .* (Lambda - mean(Lambda,1));

        % SVD step
        [U, ~, V] = svd(A_rot' * B);

        R_new = U * V';

        if norm(R - R_new, 'fro') < eps
            break
        end

        R = R_new;
    end

    A_rot = A * R;
end